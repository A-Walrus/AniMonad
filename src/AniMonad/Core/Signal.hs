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
    (|~),
    (~>),
    delay,
    simul,
    keys,
    Inner (Inner),
    mapEnd,
    inner,
    inners,
    fn,
  )
where

import Control.Exception (assert)
import Control.Lens
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

sigLens :: Traversal' a b -> Traversal' (Signal a) (Signal b)
sigLens field = traversal f
  where
    f bfb (sa :: Signal a) = ((\sbs -> (\a bs -> a & partsOf field .~ bs) <$> sa <*> sbs) <$>) $ sequenceA <$> traverse bfb (decompose $ toListOf field <$> sa)
    decompose :: Signal [b] -> [Signal b]
    decompose sigBs = [(\l -> assert (length l == len) (l !! i)) <$> sigBs | i <- [0 .. (len - 1)]]
      where
        len = length (start sigBs)

class Chainable k a where
  after :: a -> k a -> Signal a

newtype Chain a = Chain (a -> Signal a)

instance Chainable Chain a where
  after val (Chain f) = f val

instance Chainable Signal a where
  after _ s = s

data Fn a where
  Fn :: (Chainable b a) => (a -> b a) -> Fn a

fn :: (Chainable b a) => (a -> b a) -> Fn a
fn = Fn

instance Chainable Fn a where
  after val (Fn f) = after val (f val)

delay :: Time -> Chain a
delay t = Chain (\val -> Signal (const val) t)

mapEnd :: (a -> a) -> Chain a
mapEnd f = Chain (pure . f)

data Inner a where
  Inner :: (Chainable c b) => (Traversal' a b) -> c b -> Inner a

inner :: (Chainable c b) => Traversal' a b -> c b -> Inner a
inner = Inner

inners :: (Chainable c b) => Traversal' a b -> [c b] -> Inner a
inners trav cbs = inner (partsOf trav) thing
  where
    thing = Fn (\vals -> sequenceA [after val cb | val <- vals, cb <- cbs])

simul :: [Time -> Inner a] -> Time -> Keys a
simul l time = Keys (map ($ time) l)

newtype Keys a = Keys [Inner a]

keys :: [Inner a] -> Keys a
keys = Keys

instance Chainable Keys a where
  after initial (Keys l) = foldr applyInner (pure initial) l
    where
      applyInner (Inner trav c) = set (partsOf $ sigLens trav) x
        where
          x = map (`after` c) $ toListOf trav initial

instance Chainable Inner a where
  after initial a = after initial (Keys [a])

(|~) :: (Chainable k a) => a -> k a -> Signal a
initial |~ k = initial `after` k

(~>) :: (Chainable k1 a, Chainable k2 a) => k1 a -> k2 a -> Chain a
c1 ~> c2 = Chain (\val -> a val <> after (end (a val)) c2)
  where
    a val = after val c1

infixl 7 ~>

infixl 6 |~
