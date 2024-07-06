{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AniMonad
  ( module Control.Lens,
    module Linear,
    module Data.Colour.Names,
    module Data.Colour.SRGB,
    module AniMonad.Core,
    fps,
    frames,
    unframes,
    svgDoc,
    Rect (Rect),
    Circle (Circle),
    draw,
    width,
    height,
    radius,
    color,
    SomeElement (SomeElement),
    Transformed (Transformed),
    at,
    translation,
    transform,
    x,
    y,
  )
where

import AniMonad.Core
import AniMonad.TH

import Control.Lens hiding (at, children, element, transform)
import Data.Colour hiding (over)
import Data.Colour.Names
import Data.Colour.SRGB
import Data.List (intercalate)
import Data.Text (Text, pack)
import Linear (M33, V2 (V2), V3 (V3), identity)
import Linear.Matrix ((!*))
import Lucid.Svg


-- Fields

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

-- Objects
type Color = Colour Float

showColor :: Color -> Text
showColor = pack . sRGB24show

data Rect = Rect {_width, _height :: Float, _color :: Color} deriving (Show)

$(makeElementLenses ''Rect)

showT :: (Show a) => a -> Text
showT = pack . show

instance Element Rect where
  draw (Rect w h c) = rect_ [x_ (showT (-w2)), y_ (showT (-h2)), width_ (showT w), height_ (showT h), fill_ (showColor c)]
    where
      (w2, h2) = (w / 2, h / 2)

  box (Rect w h _) = BoundingBox (V2 (-w2) (-h2)) (V2 w2 h2)
    where
      (w2, h2) = (w / 2, h / 2)

data Circle = Circle {_radius :: Float, _color :: Color} deriving (Show)

$(makeElementLenses ''Circle)

instance Element Circle where
  draw (Circle r c) = circle_ [r_ rad, fill_ (showColor c)]
    where
      rad = showT r
  box (Circle r _) = BoundingBox (V2 (-r) (-r)) (V2 r r)

instance (Element a) => Element [a] where
  draw = foldr ((<>) . draw) mempty
  box = foldr1 combine . map box

$(genElementInstances 8)

data SomeElement where
  SomeElement :: (Element a) => a -> SomeElement

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

at :: (Element a) => Vec2 -> a -> Transformed a
at (V2 x y) = Transformed (V3 (V3 1 0 x) (V3 0 1 y) (V3 0 1 0))

instance Element (Transformed a) where
  draw (Transformed txform element) = g_ [transform_ transformT] (draw element)
    where
      V3 (V3 a c e) (V3 b d f) _ = txform
      transformT = pack $ "matrix(" ++ intercalate "," (map show [a, b, c, d, e, f]) ++ ")"
  box (Transformed txform element) = BoundingBox (project txform min) (project txform max)
    where
      (BoundingBox min max) = box element

project :: Transform -> Vec2 -> Vec2
project mat v = dehomogenize (mat !* homogenize v)
  where
    homogenize (V2 a b) = V3 a b 1
    dehomogenize (V3 a b _) = V2 a b

docWidth, docHeight :: Int
docWidth = 1024
docHeight = 1024

svgDoc :: Svg () -> Svg ()
svgDoc content = do
  doctype_
  with (svg11_ content) [version_ "1.1", width_ w, height_ h, viewBox_ $ nw2 <> " " <> nh2 <> " " <> w <> " " <> h]
  where
    h = showT docHeight
    w = showT docWidth
    (nh2, nw2) = (showT (-(docHeight `div` 2)), showT (-(docWidth `div` 2)))
