module AniMonad (someFunc) where

import Control.Lens

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Time = Float

-- Signal
data Signal a = Signal (Time -> a) Time

instance Functor Signal where
  fmap m (Signal f d) = Signal (m . f) d

instance Applicative Signal where
  pure a = Signal (const a) 0
  (Signal f_fn f_dur) <*> (Signal v_fn v_dur) = Signal (\t -> f_fn t $ v_fn t) (max f_dur v_dur)

-- Frame
fps :: Time
fps = 24

frame :: Time
frame = 1 / frame

frames :: Signal a -> [a]
frames = _

unframes :: [a] -> Signal a
unframes l = Signal (\t -> l !! floor (t / frame)) (fromIntegral (length l) * frame)

-- Lerp
class Lerpable a where
  lerp :: a -> a -> Time -> a

instance Lerpable Float where
  lerp a b t = (1 - t) * a + (t * b)

instance Lerpable Int where
  lerp a b = round . lerp (fromIntegral a :: Float) (fromIntegral b :: Float)

-- Keys
data Key a where
  Key :: (Lerpable b) => Lens' a b -> b -> Time -> Key a

class Keys k a where
  list :: k -> [Key a]

instance Keys [Key a] a where
  list = id

instance Keys (Key a) a where
  list key = [key]

