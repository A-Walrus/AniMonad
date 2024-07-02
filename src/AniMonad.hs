{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AniMonad (fps, frames, unframes, lerp, sigLens, extend, stretch, stretchTo, end, start, Signal, (|~), (~>), Key (Key, Key'), All (All), Ease, svgDoc, Rect (Rect), Circle (Circle), draw, width, height, radius, module Control.Lens, SomeElement (SomeElement), Transformed (Transformed), module Linear, inner, module Data.Colour.Names, module Data.Colour.SRGB, color) where

import Control.Lens hiding (children, element, transform)
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.List (intercalate)
import Data.Text (Text, pack)
import Ease
import Linear hiding (lerp)
import Lucid.Svg

import TH

type Time = Float

-- Signal
data Signal a = Signal (Time -> a) Time

instance Functor Signal where
  fmap m (Signal f d) = Signal (m . f) d

instance Applicative Signal where
  pure a = Signal (const a) 0
  f@(Signal _ f_dur) <*> v@(Signal _ v_dur) = Signal (\t -> f_fn t $ v_fn t) dur
    where
      dur = max f_dur v_dur
      Signal f_fn _ = extend dur f
      Signal v_fn _ = extend dur v

end :: Signal a -> a
end (Signal fn dur) = fn dur

start :: Signal a -> a
start (Signal fn _) = fn 0

extend :: Time -> Signal a -> Signal a
extend time (Signal f d) = Signal (\t -> if t < d then f t else f d) (max time d)

stretch :: Float -> Signal a -> Signal a
stretch fac (Signal f d) = Signal (f . (/ fac)) (d * fac)

ease :: Ease Time -> Signal a -> Signal a
ease easing (Signal f d) = Signal (\t -> f $ d * easing (t / d)) d

stretchTo :: Float -> Signal a -> Signal a
stretchTo time (Signal f d) = Signal (f . (/ time) . (* d)) time

instance Semigroup (Signal a) where
  (Signal a_fn a_dur) <> (Signal b_fn b_dur) = Signal (\t -> if t < a_dur then a_fn t else b_fn (t - a_dur)) (a_dur + b_dur)

instance Monoid (Signal a) where
  mempty = pure undefined

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
  lerp' :: a -> a -> Time -> a
  lerp a b = Signal (lerp' a b) 1

instance Lerp Float where
  lerp' a b t = (1 - t) * a + (t * b)

instance Lerp Int where
  lerp' a b = round . lerp' (fromIntegral a :: Float) (fromIntegral b :: Float)

instance Lerp Color where
  lerp' a b t = blend t b a

-- Keys
data Key a where
  Key :: (Lerp b) => Lens' a b -> b -> Time -> Key a
  Key' :: (Lerp b) => Lens' a b -> b -> Ease Float -> Time -> Key a

class Keys k a | k -> a where
  list :: k -> [Key a]

instance Keys [Key a] a where
  list = id

data All a = All [Time -> Key a] Time

instance Keys (All a) a where
  list (All keys time) = map ($ time) keys

instance Keys (Key a) a where
  list key = [key]

(|~) :: (Keys k a) => a -> k -> Signal a
initial |~ k = foldr thing (pure initial) keys
  where
    keys = list k
    thing (Key field val time) = thing (Key' field val cubicInOut time)
    thing (Key' field val easing time) = set (sigLens field) (stretch time $ ease easing $ lerp (initial ^. field) val)

(~>) :: (Keys k a) => Signal a -> k -> Signal a
signal ~> k = signal <> (end signal |~ keys)
  where
    keys = list k

-- Objects
type Color = Colour Float

showColor :: Color -> Text
showColor = pack . sRGB24show

data Rect = Rect {_width, _height :: Float, _rectColor :: Color} deriving (Show)

$(makeLenses ''Rect)
$(makeFields ''Rect)

showT :: (Show a) => a -> Text
showT = pack . show

instance Element Rect where
  draw (Rect w h c) = rect_ [x_ (showT (-w2)), y_ (showT (-h2)), width_ (showT w), height_ (showT h), fill_ (showColor c)]
    where
      (w2, h2) = (w / 2, h / 2)

  box (Rect w h _) = BoundingBox (V2 (-w2) (-h2)) (V2 w2 h2)
    where
      (w2, h2) = (w / 2, h / 2)

data Circle = Circle {_radius :: Float, _circleColor :: Color} deriving (Show)

$(makeLenses ''Circle)
$(makeFields ''Circle)

instance Element Circle where
  draw (Circle r c) = circle_ [r_ rad, fill_ (showColor c)]
    where
      rad = showT r
  box (Circle r _) = BoundingBox (V2 (-r) (-r)) (V2 r r)

type Transform = M33 Float

data Transformed a = (Element a) => Transformed Transform a

inner :: Lens' (Transformed a) a
inner = lens (\(Transformed _ a) -> a) (\(Transformed t _) a -> Transformed t a)

transform :: Lens' (Transformed a) Transform
transform = lens (\(Transformed t _) -> t) (\(Transformed _ a) t -> Transformed t a)

instance Element (Transformed a) where
  draw (Transformed txform element) = g_ [transform_ transformT] (draw element)
    where
      V3 (V3 a c e) (V3 b d f) _ = txform
      transformT = pack $ "matrix(" ++ intercalate "," (map show [a, b, c, d, e, f]) ++ ")"
  box = undefined -- TODO

$(genElementInstances 8)

data SomeElement where
  SomeElement :: (Element a) => a -> SomeElement

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
