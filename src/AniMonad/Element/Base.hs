{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AniMonad.Element.Base (module Data.Colour.Names, V2 (V2), Vec2, Color, BoundingBox (BoundingBox), Transformed, Element (box, draw), showColor, val, combine, transform, Transform, project, at, x, y, translation,showT) where

import Control.Lens (Lens')
import Control.Lens.Combinators (lens)
import Data.Colour (Colour)
import Data.Colour.Names
import Data.Colour.SRGB (sRGB24show)
import Data.List (intercalate)
import Data.Text (Text, pack)
import Linear (V2 (V2), V3 (V3))
import Linear.Matrix (M33, (!*))
import Lucid.Svg

type Vec2 = V2 Float

type Color = Colour Float

showT :: (Show a) => a -> Text
showT = pack . show

showColor :: Color -> Text
showColor = pack . sRGB24show

class Element a where
  draw :: a -> Svg ()
  box :: a -> BoundingBox

data BoundingBox = BoundingBox Vec2 Vec2

combine :: BoundingBox -> BoundingBox -> BoundingBox
combine (BoundingBox (V2 ax1 ay1) (V2 ax2 ay2)) (BoundingBox (V2 bx1 by1) (V2 bx2 by2)) = BoundingBox (V2 (min ax1 bx1) (min ay1 by1)) (V2 (max ax2 bx2) (max ay2 by2))

type Transform = M33 Float

data Transformed a = (Element a) => Transformed {_transform :: Transform, _val :: a}

val :: Lens' (Transformed a) a
val = lens (\(Transformed _ a) -> a) (\(Transformed t _) a -> Transformed t a)

transform :: Lens' (Transformed a) Transform
transform = lens (\(Transformed t _) -> t) (\(Transformed _ a) t -> Transformed t a)

project :: Transform -> Vec2 -> Vec2
project mat v = dehomogenize (mat !* homogenize v)
  where
    homogenize (V2 a b) = V3 a b 1
    dehomogenize (V3 a b _) = V2 a b

at :: (Element a) => Vec2 -> a -> Transformed a
at (V2 x y) = Transformed (V3 (V3 1 0 x) (V3 0 1 y) (V3 0 1 0))

class HasTranslation a where
  translation :: Lens' a Vec2

instance HasTranslation Transform where
  translation = lens v s
    where
      v (V3 (V3 _ _ x) (V3 _ _ y) _) = V2 x y
      s (V3 (V3 a b _) (V3 d e _) (V3 h i g)) (V2 x y) = V3 (V3 a b x) (V3 d e y) (V3 h i g)

instance HasTranslation (Transformed a) where
  translation = transform . translation

class HasXY a where
  x :: Lens' a Float
  y :: Lens' a Float

instance HasXY Vec2 where
  x = lens (\(V2 x _) -> x) (\(V2 _ y) x -> V2 x y)
  y = lens (\(V2 _ y) -> y) (\(V2 x _) y -> V2 x y)

instance HasXY Transform where
  x = translation . x
  y = translation . y

instance HasXY (Transformed a) where
  x = transform . x
  y = transform . y

instance Element (Transformed a) where
  draw (Transformed txform element) = g_ [transform_ transformT] (draw element)
    where
      V3 (V3 a c e) (V3 b d f) _ = txform
      transformT = pack $ "matrix(" ++ intercalate "," (map show [a, b, c, d, e, f]) ++ ")"
  box (Transformed txform element) = BoundingBox (project txform min) (project txform max)
    where
      (BoundingBox min max) = box element
