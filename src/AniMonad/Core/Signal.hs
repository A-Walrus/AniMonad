module AniMonad.Core.Signal
  ( Time,
    Signal (Signal),
    frameTime,
    frames,
    end,
    start,
    extend,
    delay,
    simul,
    keys,
    Chain (Chain),
    chain,
    signal,
    (|>),
    mapEnd,
    inner,
    innerFn,
    inners,
    fn,
    asFn,
    sample,
    duration,
    sigLens,
    sigSet,
    boomerang,
  )
where

import AniMonad.Config
import Control.Exception (assert)
import Control.Lens hiding ((|>))
import Control.Monad (zipWithM)

type Time = Float

type Duration = Int

toDur :: (?config :: Config) => Time -> Duration
toDur t = round (t * fromIntegral (fps ?config))

frameTime :: (?config :: Config) => Time
frameTime = 1 / fromIntegral (fps ?config)

newtype Signal a = Signal [a] deriving (Show, Eq)

sample :: (?config :: Config) => Time -> (Time -> a) -> Signal a
sample time f = Signal $ map f [0, frameTime .. time]

frames :: Signal a -> [a]
frames (Signal f) = init f

duration :: Signal a -> Int
duration (Signal l) = length l - 1

instance Functor Signal where
  fmap m (Signal l) = Signal (fmap m l)

instance Applicative Signal where
  pure a = Signal [a]
  a <*> b = Signal (zipWith ($) new_a new_b)
    where
      dur = max (duration a) (duration b)
      Signal new_a = extend dur a
      Signal new_b = extend dur b

instance Semigroup (Signal a) where
  (Signal a) <> (Signal b) = Signal (init a ++ b)

end :: Signal a -> a
end (Signal l) = last l

start :: Signal a -> a
start (Signal l) = head l

extend :: Int -> Signal a -> Signal a
extend new_dur s = s <> Signal (replicate missing_frames (end s))
  where
    missing_frames = new_dur - duration s + 1

sigReverse :: Signal a -> Signal a
sigReverse (Signal l) = Signal (reverse l)

sigSet :: Traversal' a b -> Signal b -> Signal a -> Signal a
sigSet trav = set (sigLens trav)

sigLens :: Traversal' a b -> Traversal' (Signal a) (Signal b)
sigLens field = traversal f
  where
    f bfb sa = ((\sbs -> (\a bs -> a & partsOf field .~ bs) <$> sa <*> sbs) <$>) $ sequenceA <$> traverse bfb (decompose $ toListOf field <$> sa)
    decompose :: Signal [b] -> [Signal b]
    decompose sigBs = [(\l -> assert (length l == len) (l !! i)) <$> sigBs | i <- [0 .. (len - 1)]]
      where
        len = length (start sigBs)

data Chain a where
  Chain :: (Traversal' a b) -> (b -> Signal b) -> Chain a

instance Semigroup (Chain a) where
  (<>) :: Chain a -> Chain a -> Chain a
  c1 <> c2 = chain (\val -> a val <> asFn c2 (end (a val)))
    where
      a = asFn c1

instance Monoid (Chain a) where
  mempty = mapEnd id

inner :: Traversal' a b -> Chain b -> Chain a
inner t x = Chain t (asFn x)

innerFn :: Traversal' a b -> (b -> Chain b) -> Chain a
innerFn t x = inner t (fn x)

inners :: Traversal' a b -> [Chain b] -> Chain a
inners trav cbs = inner (partsOf trav) (chain thing)
  where
    thing = zipWithM asFn cbs

simul :: [Time -> Chain a] -> Time -> Chain a
simul l time = keys (map ($ time) l)

keys :: [Chain a] -> Chain a
keys l = chain thing
  where
    thing initial = foldr applyChain (pure initial) l

chain :: (a -> Signal a) -> Chain a
chain = Chain id

signal :: Signal a -> Chain a
signal = chain . const

fn :: (a -> Chain a) -> Chain a
fn f = chain (\x -> asFn (f x) x)

boomerang :: Chain a -> Chain a
boomerang (Chain trav f) = Chain trav (boomerang' . f)
  where
    boomerang' sig = sig <> sigReverse sig

delay :: (?config :: Config) => Time -> Chain a
delay t = chain (Signal . replicate (1 + toDur t))

mapEnd :: (a -> a) -> Chain a
mapEnd f = chain (pure . f)

applyChain :: Chain a -> Signal a -> Signal a
applyChain (Chain trav c) s = set (partsOf $ sigLens trav) x s
  where
    x = map c $ toListOf trav (start s)

asFn :: Chain a -> a -> Signal a
asFn c initial = applyChain c (pure initial)

(|>) :: a -> Chain a -> Signal a
(|>) = flip asFn

infixl 5 |>
