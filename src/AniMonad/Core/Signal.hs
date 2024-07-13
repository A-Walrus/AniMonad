{-# LANGUAGE MonoLocalBinds #-}

module AniMonad.Core.Signal
  ( Time,
    Signal (Signal),
    fps,
    frameTime,
    end,
    start,
    extend,
    delay,
    simul,
    keys,
    Chain (Chain),
    chain,
    (|>),
    mapEnd,
    inner,
    innerFn,
    inners,
    fn,
    asFn,
    sample,
    duration,
    applyChain,
    sigLens,
  )
where

import Control.Exception (assert)
import Control.Lens hiding ((|>))

type Time = Float

type Duration = Int

toDur :: Time -> Duration
toDur t = round (t / frameTime)

sample :: Time -> (Time -> a) -> Signal a
sample time f = Signal $ map f [0, frameTime .. time]

fps :: Int
fps = 24

frameTime :: Time
frameTime = 1 / fromIntegral fps

newtype Signal a = Signal [a] deriving (Show, Eq)

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

-- stretchBy :: Float -> Signal a -> Signal a
-- stretchBy fac (Signal f d) = Signal (f . (/ fac)) (d * fac)

-- stretchTo :: Float -> Signal a -> Signal a
-- stretchTo time (Signal f d) = Signal (f . (/ time) . (* d)) time

-- ease :: Ease Time -> Signal a -> Signal a
-- ease easing (Signal l) = Signal (\t -> f $ d * easing (t / d)) d

-- unsample :: Time -> [a] -> Signal a
-- unsample step l = Signal f (fromIntegral (length l) * step)
--   where
--     f t = l !! min (floor (t / step)) (length l - 1)

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
    thing vals = sequenceA [asFn cb val | val <- vals, cb <- cbs]

simul :: [Time -> Chain a] -> Time -> Chain a
simul l time = keys (map ($ time) l)

keys :: [Chain a] -> Chain a
keys l = chain thing
  where
    thing initial = foldr applyChain (pure initial) l

chain :: (a -> Signal a) -> Chain a
chain = Chain id

fn :: (a -> Chain a) -> Chain a
fn f = chain (\x -> asFn (f x) x)

delay :: Time -> Chain a
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
