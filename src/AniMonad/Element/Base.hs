{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AniMonad.Element.Base
  ( module Data.Colour.Names,
    sRGB24read,
    V2 (V2),
    Vec2,
    Color,
    BoundingBox (BoundingBox),
    SomeElem (SomeElem),
    as,
    boxWidth,
    boxHeight,
    Transformed,
    Element (box, draw),
    showColor,
    val,
    combine,
    transform,
    transformed,
    Transform,
    project,
    at,
    x,
    y,
    translation,
    rotationR,
    rotation,
    showT,
    overlay,
    Overlay (Overlay),
    ComplexElem (realize),
    thing,
    makeOverlay,
  )
where

import AniMonad.Core.Signal (Action (Action))
import Control.Lens (Lens', makeLensesFor)
import Control.Lens.Combinators (lens)
import Data.Colour (Colour)
import Data.Colour.Names
import Data.Colour.SRGB (sRGB24read, sRGB24show)
import Data.Data (Typeable, cast)
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

data BoundingBox = BoundingBox Vec2 Vec2 deriving (Show)

boxWidth :: BoundingBox -> Float
boxWidth (BoundingBox (V2 a _) (V2 b _)) = b - a

boxHeight :: BoundingBox -> Float
boxHeight (BoundingBox (V2 _ a) (V2 _ b)) = b - a

corners :: BoundingBox -> [Vec2]
corners (BoundingBox min@(V2 a b) max@(V2 c d)) = [min, V2 a d, max, V2 c b]

point :: Vec2 -> BoundingBox
point v = BoundingBox v v

data SomeElem where
  SomeElem :: (Element a, Typeable a) => a -> SomeElem

instance Element SomeElem where
  draw (SomeElem e) = draw e
  box (SomeElem e) = box e

castElem :: forall a. (Element a, Typeable a) => SomeElem -> Maybe a
castElem (SomeElem a) = cast a

as :: forall a. (Element a, Typeable a) => Lens' SomeElem a
as = lens v o
  where
    v s = case castElem s of
      Just x -> x
      Nothing -> error "`as` cast failed. Element is of incorrect type."
    o _ = SomeElem

combine :: BoundingBox -> BoundingBox -> BoundingBox
combine box (BoundingBox a b) = grow (grow box a) b

grow :: BoundingBox -> Vec2 -> BoundingBox
grow (BoundingBox (V2 ax1 ay1) (V2 ax2 ay2)) (V2 bx by) = BoundingBox (V2 (min ax1 bx) (min ay1 by)) (V2 (max ax2 bx) (max ay2 by))

data Transform = Transform {_position :: Vec2, _rotation :: Float, _scale :: Vec2} deriving (Show)

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
asMat (Transform {_position, _rotation, _scale}) = scaleMat _scale !*! translationMat _position !*! rotationMat _rotation

data Transformed a = (Element a) => Transformed {_transform :: Transform, _val :: a}

instance (Show a) => Show (Transformed a) where
  show (Transformed t v) = "Transformed " ++ show t ++ " " ++ show v

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

transformed :: (Element a) => a -> Transformed a
transformed = Transformed identity

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
  box (Transformed txform element) = foldl grow (point (head newCorners)) newCorners
    where
      newCorners = map (project txform) $ corners $ box element

-- Complex Element

class (Element b) => ComplexElem a b | a -> b where
  realize :: a -> b

data Overlay a b = (ComplexElem a b) => Overlay {_inner :: a, _fn :: b -> b}

thing :: Lens' (Overlay a b) a
thing = lens _inner s
  where
    s x y = x {_inner = y}

overlay :: (ComplexElem a b) => a -> Overlay a b
overlay x = Overlay x id

instance Element (Overlay a b) where
  draw (Overlay a f) = draw . f $ realize a
  box (Overlay a f) = box . f $ realize a

makeOverlay :: Action b -> Action (Overlay a b)
makeOverlay (Action a) = Action f
  where
    f (Overlay val _) = (\f (Overlay a b) -> Overlay a (b . f)) <$> x
      where
        x = a (realize val)
