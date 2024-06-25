{-# LANGUAGE FunctionalDependencies #-}

module AniMonad (frames, unframes, lerp, sigLens, extend, stretch, stretchTo, end, start, Signal, (|~), (~>), Key (Key) ) where

import Control.Lens

type Time = Float

-- Signal
data Signal a = Signal (Time -> a) Time

instance Functor Signal where
  fmap m (Signal f d) = Signal (m . f) d

instance Applicative Signal where
  pure a = Signal (const a) 0
  (Signal f_fn f_dur) <*> (Signal v_fn v_dur) = Signal (\t -> f_fn t $ v_fn t) (max f_dur v_dur)

end :: Signal a -> a
end (Signal fn dur) = fn dur

start :: Signal a -> a
start (Signal fn _) = fn 0

extend :: Time -> Signal a -> Signal a
extend time (Signal f d) = Signal (\t -> if t < d then f time else f d) (max time d)

stretch :: Float -> Signal a -> Signal a
stretch fac (Signal f d) = Signal (f . (/ fac)) (d * fac)

stretchTo :: Float -> Signal a -> Signal a
stretchTo time (Signal f d) = Signal (f . (/ time) . (* d)) time

instance Semigroup (Signal a) where
  (Signal a_fn a_dur) <> (Signal b_fn b_dur) = Signal (\t -> if t < a_dur then a_fn t else b_fn t) (a_dur + b_dur)

instance Monoid (Signal a) where
  mempty = pure undefined

-- Fields
animateField :: a -> Lens' a b -> Signal b -> Signal a
animateField initial field signal = set (sigLens field) signal (pure initial)

sigLens :: Lens' a b -> Lens' (Signal a) (Signal b)
sigLens field = lens v o
  where
    v sig = view field <$> sig
    o sig sub_sig = set field <$> sub_sig <*> sig

-- Frame
fps :: Time
fps = 24

frame :: Time
frame = 1 / fps

frames :: Signal a -> [a]
frames (Signal f dur) = map f [0, frame .. (dur - frame)]

unframes :: [a] -> Signal a
unframes = unframes' frame

unframes' :: Time -> [a] -> Signal a
unframes' step l = Signal f (fromIntegral (length l) * step)
  where
    f t = l !! min (floor (t / step)) (length l - 1)

-- Lerp
class Lerp a where
  lerp :: a -> a -> Signal a

instance Lerp Float where
  lerp a b = Signal (\t -> (1 - t) * a + (t * b)) 1

instance Lerp Int where
  lerp a b = round <$> lerp (fromIntegral a :: Float) (fromIntegral b :: Float)

-- Keys
data Key a where
  Key :: (Lerp b) => Lens' a b -> b -> Time -> Key a

class Keys k a | k -> a where
  list :: k -> [Key a]

instance Keys [Key a] a where
  list = id

instance Keys (Key a) a where
  list key = [key]

(|~) :: (Keys k a) => a -> k -> Signal a
initial |~ k = foldr thing (pure initial) keys
  where
    keys = list k
    thing (Key field val time) = set (sigLens field) (stretch time $ lerp (initial ^. field) val)

(~>) :: (Keys k a) => Signal a -> k -> Signal a
signal ~> k = signal <> (end signal |~ keys)
  where
    keys = list k
