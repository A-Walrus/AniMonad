module AniMonad (frames, unframes, lerp, sigLens, Signal) where

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

chain :: Signal a -> Signal a -> Signal a
chain (Signal a_fn a_dur) (Signal b_fn b_dur) = Signal (\t -> if t < a_dur then a_fn t else b_fn t) (a_dur + b_dur)

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

class Keys k a where
  list :: k -> [Key a]

instance Keys [Key a] a where
  list = id

instance Keys (Key a) a where
  list key = [key]
