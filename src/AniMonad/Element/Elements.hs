{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AniMonad.Element.Elements where

import AniMonad.Element.Base
import AniMonad.Element.TH
import Lucid.Svg

$(genElementInstances 8) -- Element tuples

data SomeElement where
  SomeElement :: (Element a) => a -> SomeElement

data Rect = Rect {_width, _height :: Float, _color :: Color} deriving (Show)

$(makeElementLenses ''Rect)

instance (Element a) => Element [a] where
  draw = foldr ((<>) . draw) mempty
  box = foldr1 combine . map box

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
