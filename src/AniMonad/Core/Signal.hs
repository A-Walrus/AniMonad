module AniMonad.Core.Signal
  ( Time,
    Signal (Signal),
    frameTime,
    frames,
    toDur,
    end,
    start,
    extend,
    delay,
    simul,
    keys,
    Action (Action),
    signal,
    (|>),
    mapEnd,
    inner,
    innerFn,
    inners,
    fn,
    sample,
    duration,
    sigLens,
    sigSet,
    boomerang,
  )
where

import AniMonad.Config
import Control.Exception (assert)
import Control.Lens hiding (both, (|>))
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

newtype Action a = Action {action :: a -> Signal (a -> a)}

instance Semigroup (Action a) where
  (<>) :: Action a -> Action a -> Action a
  (Action a) <> (Action b) = Action f
    where
      f val = a val <> ((. endFn) <$> b endVal)
        where
          endFn = end (a val)
          endVal = endFn val

instance Monoid (Action a) where
  mempty = mapEnd id

inner :: Traversal' a b -> Action b -> Action a
inner t (Action f) = Action (\val -> thing <$> traverse f (toListOf t val))
  where
    thing funcs val = set (partsOf t) (zipWith ($) funcs (toListOf t val)) val

innerFn :: Traversal' a b -> (b -> Action b) -> Action a
innerFn t x = inner t (fn x)

inners :: Traversal' a b -> [Action b] -> Action a
inners trav cbs = inner (partsOf trav) (Action thing)
  where
    thing = (zipWith ($) <$>) . zipWithM action cbs

simul :: [Time -> Action a] -> Time -> Action a
simul l time = keys (map ($ time) l)

keys :: [Action a] -> Action a
keys = foldr1 both
  where
    both (Action a) (Action b) = Action (\val -> (.) <$> b val <*> a val)

signal :: Signal a -> Action a
signal sig = Action (const $ const <$> sig)

fn :: (a -> Action a) -> Action a
fn f = Action (\x -> action (f x) x)

boomerang :: Action a -> Action a
boomerang (Action f) = Action (\s -> f s <> sigReverse (f s))

delay :: (?config :: Config) => Time -> Action a
delay t = Action (const (sample t (const id)))

mapEnd :: (a -> a) -> Action a
-- mapEnd f = Action ((const . pure) f)
mapEnd f = Action (const $ pure f)

applyAction :: Action a -> Signal a -> Signal a
applyAction (Action f) sig = f (start sig) <*> sig

(|>) :: a -> Action a -> Signal a
val |> action = applyAction action (pure val)

infixl 5 |>
