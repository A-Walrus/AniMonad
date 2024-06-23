module AniMonad (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


type Time = Float


fps :: Time
fps = 24
frame :: Time
frame = 1/frame

data Signal a = Signal (Time -> a) Time

instance Functor Signal where
  fmap m (Signal f d) = Signal (m . f) d

instance Applicative Signal where
  pure a = Signal (const a) 0
  (Signal f_fn f_dur) <*> (Signal v_fn v_dur) = Signal (\t -> f_fn t $ v_fn t) (max f_dur v_dur)
