{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}

module AniMonad.Core.Signal
  ( Time,
    Signal (Signal),
    end,
    start,
    extend,
    stretchBy,
    stretchTo,
    ease,
    sigLens,
    delay,
    simul,
    keys,
    Chain (Chain),
    (|>),
    mapEnd,
    inner,
    inners,
    fn,
    sample,
    unsample,
  )
where

import Control.Exception (assert)
import Control.Lens hiding ((|>))
import Ease

type Time = Float

data Signal a = Signal (Time -> a) Time

instance Functor Signal where
  fmap m (Signal f d) = Signal (m . f) d

instance Applicative Signal where
  pure a = Signal (const a) 0
  f@(Signal _ f_dur) <*> v@(Signal _ v_dur) = Signal (\t -> f_fn t $ v_fn t) dur
    where
      dur = max f_dur v_dur
      Signal f_fn _ = extend dur f
      Signal v_fn _ = extend dur v

instance Semigroup (Signal a) where
  (Signal a_fn a_dur) <> (Signal b_fn b_dur) = Signal (\t -> if t < a_dur then a_fn t else b_fn (t - a_dur)) (a_dur + b_dur)

end :: Signal a -> a
end (Signal f dur) = f dur

start :: Signal a -> a
start (Signal f _) = f 0

extend :: Time -> Signal a -> Signal a
extend time (Signal f d) = Signal (\t -> if t < d then f t else f d) (max time d)

stretchBy :: Float -> Signal a -> Signal a
stretchBy fac (Signal f d) = Signal (f . (/ fac)) (d * fac)

stretchTo :: Float -> Signal a -> Signal a
stretchTo time (Signal f d) = Signal (f . (/ time) . (* d)) time

ease :: Ease Time -> Signal a -> Signal a
ease easing (Signal f d) = Signal (\t -> f $ d * easing (t / d)) d

sample :: Time -> Signal a -> [a]
sample step (Signal f dur) = map f [0, step .. (dur - step)]

unsample :: Time -> [a] -> Signal a
unsample step l = Signal f (fromIntegral (length l) * step)
  where
    f t = l !! min (floor (t / step)) (length l - 1)

sigLens :: Traversal' a b -> Traversal' (Signal a) (Signal b)
sigLens field = traversal f
  where
    f bfb (sa :: Signal a) = ((\sbs -> (\a bs -> a & partsOf field .~ bs) <$> sa <*> sbs) <$>) $ sequenceA <$> traverse bfb (decompose $ toListOf field <$> sa)
    decompose :: Signal [b] -> [Signal b]
    decompose sigBs = [(\l -> assert (length l == len) (l !! i)) <$> sigBs | i <- [0 .. (len - 1)]]
      where
        len = length (start sigBs)

data Chain a where
  Chain :: (Traversal' a b) -> (b -> Signal b) -> Chain a

instance Semigroup (Chain a) where
  (<>) :: Chain a -> Chain a -> Chain a
  c1 <> c2 = chain (\val -> a val <> after (end (a val)) c2)
    where
      a val = after val c1

instance Monoid (Chain a) where
  mempty = chain pure

chain :: (a -> Signal a) -> Chain a
chain = inner id

fn :: (a -> Chain a) -> Chain a
fn f = chain (into f)

delay :: Time -> Chain a
delay t = chain (\val -> Signal (const val) t)

mapEnd :: (a -> a) -> Chain a
mapEnd f = chain (pure . f)

class ChainFn c a | c -> a where
  into :: c -> (a -> Signal a)

instance ChainFn (a -> Signal a) a where
  into = id

instance ChainFn (Chain a) a where
  into i = (`after` i)

instance ChainFn (a -> Chain a) a where
  into f v = after v (f v)

inner :: (ChainFn c b) => Traversal' a b -> c -> Chain a
inner t x = Chain t (into x)

inners :: Traversal' a b -> [Chain b] -> Chain a
inners trav cbs = inner (partsOf trav) thing
  where
    thing vals = sequenceA [after val cb | val <- vals, cb <- cbs]

simul :: [Time -> Chain a] -> Time -> Chain a
simul l time = keys (map ($ time) l)

keys :: [Chain a] -> Chain a
keys l = chain thing
  where
    thing initial = foldr applyChain (pure initial) l

applyChain :: Chain a -> Signal a -> Signal a
applyChain (Chain trav c) s = set (partsOf $ sigLens trav) x s
  where
    x = map c $ toListOf trav (start s)

after :: a -> Chain a -> Signal a
initial `after` inn = applyChain inn (pure initial)

(|>) :: a -> Chain a -> Signal a
(|>) = after

infixl 5 |>
