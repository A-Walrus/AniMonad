module AniMonad.Core.Lerp where

import AniMonad.Core.Signal (Signal (Signal), Time)
import AniMonad.Element.Base (Color, Vec2)
import Data.Colour (blend)
import qualified Linear

class Lerp a where
  lerp :: a -> a -> Signal a
  lerp' :: a -> a -> Time -> a
  lerp a b = Signal (lerp' a b) 1

instance Lerp Float where
  lerp' a b t = (1 - t) * a + (t * b)

instance Lerp Int where
  lerp' a b = round . lerp' (fromIntegral a :: Float) (fromIntegral b :: Float)

instance Lerp Color where
  lerp' a b t = blend t b a

instance Lerp Vec2 where
  lerp' a b t = Linear.lerp t a b

instance (Lerp a) => Lerp [a] where
  lerp' as bs t = zipWith (\a b -> lerp' a b t) as bs
