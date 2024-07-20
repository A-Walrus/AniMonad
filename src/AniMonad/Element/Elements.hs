{-# OPTIONS_GHC -Wno-orphans #-}

module AniMonad.Element.Elements where

import AniMonad.Element.Base
import AniMonad.Element.TH
import Control.Lens.Combinators
import Data.Text (pack)
import Lucid.Svg

$(genElementInstances 8) -- Element tuples
$(genTransformTuples 8) -- Element tuples

data Rect = Rect {_width, _height :: Float, _color :: Color, _cornerRadius :: Float} deriving (Show)

$(makeElementLenses ''Rect)

instance (Element a) => Element [a] where
  draw = foldr ((<>) . draw) mempty
  box = foldr1 combine . map box

instance Element Rect where
  draw (Rect w h c r) = rect_ [x_ (showT (-w2)), y_ (showT (-h2)), width_ (showT w), height_ (showT h), fill_ (showColor c), rx_ (showT r)]
    where
      (w2, h2) = (w / 2, h / 2)

  box (Rect w h _ _) = BoundingBox (V2 (-w2) (-h2)) (V2 w2 h2)
    where
      (w2, h2) = (w / 2, h / 2)

data Circle = Circle {_radius :: Float, _color :: Color} deriving (Show)

$(makeElementLenses ''Circle)

instance Element Circle where
  draw (Circle r c) = circle_ [r_ rad, fill_ (showColor c)]
    where
      rad = showT r
  box (Circle r _) = BoundingBox (V2 (-r) (-r)) (V2 r r)

data Text a = Text
  { _str :: a,
    _fontSize :: Float, -- TODO custom size type?
    _color :: Color
  }
  deriving (Show)

$(makeElementLenses ''Text)

instance (Show a) => Element (Text a) where
  draw (Text {_str, _fontSize, _color}) = text_ [font_size_ (showT _fontSize), fill_ (showColor _color), font_family_ "monospace", text_anchor_ "middle", dominant_baseline_ "central"] (toHtml (pack (show _str))) -- TODO font family, anchor
  box _ = BoundingBox (V2 0 0) (V2 0 0) -- TODO
