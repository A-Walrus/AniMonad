{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AniMonad.Element.Base (module Data.Colour.Names, V2 (V2), Vec2, Color, BoundingBox (BoundingBox), Transformed, Element (box, draw), showColor, val, combine, transform, Transform, project, at, x, y, translation, rotationR, rotation, showT) where

import Control.Lens (Lens', makeLensesFor)
import Control.Lens.Combinators (lens)
import Data.Colour (Colour)
import Data.Colour.Names
import Data.Colour.SRGB (sRGB24show)
import Data.List (intercalate)
import Data.Text (Text, pack)
import Linear (V2 (V2), V3 (V3), (!*!))
import Linear.Matrix (M33, (!*))
import Lucid.Svg hiding (scale)

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

data Transform = Transform {_position :: Vec2, _rotation :: Float, _scale :: Vec2}

$(makeLensesFor [("_position", "_position'"), ("_rotation", "_rotation'"), ("_scale", "_scale'")] ''Transform)

type Matrix = M33 Float

identity :: Transform
identity = Transform {_position = V2 0 0, _rotation = 0, _scale = V2 1 1}

translationMat :: Vec2 -> Matrix
translationMat (V2 x y) = V3 (V3 1 0 x) (V3 0 1 y) (V3 0 0 1)

rotationMat :: Radians -> Matrix
rotationMat angle = V3 (V3 (cos angle) (-sin angle) 0) (V3 (sin angle) (cos angle) 0) (V3 0 0 1)

scaleMat :: Vec2 -> Matrix
scaleMat (V2 x y) = V3 (V3 x 0 0) (V3 0 y 0) (V3 0 0 1)

asMat :: Transform -> Matrix
asMat (Transform {_position, _rotation, _scale}) =  scaleMat _scale !*! translationMat _position !*! rotationMat _rotation

data Transformed a = (Element a) => Transformed {_transform :: Transform, _val :: a}

val :: Lens' (Transformed a) a
val = lens (\(Transformed _ a) -> a) (\(Transformed t _) a -> Transformed t a)

transform :: Lens' (Transformed a) Transform
transform = lens (\(Transformed t _) -> t) (\(Transformed _ a) t -> Transformed t a)

project :: Transform -> Vec2 -> Vec2
project txform v = dehomogenize (asMat txform !* homogenize v)
  where
    homogenize (V2 a b) = V3 a b 1
    dehomogenize (V3 a b _) = V2 a b

at :: (Element a) => Vec2 -> a -> Transformed a
at pos = Transformed (identity {_position = pos})

class HasTranslation a where
  translation :: Lens' a Vec2

instance HasTranslation Transform where
  translation = _position'

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

-- Rotation in Radians
type Radians = Float

-- Rotation in Radians
type Degrees = Float

class HasRotation a where
  rotationR :: Lens' a Radians
  rotation :: Lens' a Degrees
  rotation = rotationR . lens (* (180 / pi)) (\_ d -> d * pi / 180)

instance HasRotation Transform where
  rotationR = _rotation'

class HasScale a where
  scale :: Lens' a Vec2

instance HasScale Transform where
  scale = _scale'

instance HasRotation (Transformed a) where
  rotationR = transform . rotationR

instance HasScale (Transformed a) where
  scale = transform . scale

instance HasTranslation (Transformed a) where
  translation = transform . translation

instance Element (Transformed a) where
  draw (Transformed txform element) = g_ [transform_ transformT] (draw element)
    where
      V3 (V3 a c e) (V3 b d f) _ = asMat txform
      transformT = pack $ "matrix(" ++ intercalate "," (map show [a, b, c, d, e, f]) ++ ")"
  box (Transformed txform element) = BoundingBox (project txform min) (project txform max)
    where
      (BoundingBox min max) = box element
