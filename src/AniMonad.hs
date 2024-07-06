{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
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
    lerp,
    sigLens,
    extend,
    stretch,
    stretchTo,
    end,
    start,
    Signal (Signal),
    (|~),
    (~>),
    delay,
    simul,
    keys,
    key,
    key',
    ky,
    ky',
    mapEnd,
    inner,
    inners,
    fn,
    Ease,
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
import Control.Exception (assert)
import Control.Lens hiding (at, children, element, transform)
import Data.Colour hiding (over)
import Data.Colour.Names
import Data.Colour.SRGB
import Data.List (intercalate)
import Data.Text (Text, pack)
import Ease
import Linear (M33, V2 (V2), V3 (V3), identity)
import Linear qualified
import Linear.Matrix ((!*))
import Lucid.Svg

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

-- Fields
sigLens :: Traversal' a b -> Traversal' (Signal a) (Signal b)
sigLens field = traversal fn
  where
    fn bfb (sa :: Signal a) = ((\sbs -> (\a bs -> a & partsOf field .~ bs) <$> sa <*> sbs) <$>) $ sequenceA <$> traverse bfb (decompose $ toListOf field <$> sa)
    decompose :: Signal [b] -> [Signal b]
    decompose sigBs = [(\l -> assert (length l == len) (l !! i)) <$> sigBs | i <- [0 .. (len - 1)]]
      where
        len = length (start sigBs)

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

instance Lerp Vec2 where
  lerp' a b t = Linear.lerp t a b

instance (Lerp a) => Lerp [a] where
  lerp' as bs t = zipWith (\a b -> lerp' a b t) as bs

-- Keys
class Chainable k a where
  after :: a -> k a -> Signal a

newtype Chain a = Chain (a -> Signal a)

instance Chainable Chain a where
  after val (Chain fn) = fn val

instance Chainable Signal a where
  after _ s = s

data Fn a where
  Fn :: (Chainable b a) => (a -> b a) -> Fn a

fn :: (Chainable b a) => (a -> b a) -> Fn a
fn = Fn

instance Chainable Fn a where
  after val (Fn f) = after val (f val)

delay :: Time -> Chain a
delay t = Chain (\val -> Signal (const val) t)

mapEnd :: (a -> a) -> Chain a
mapEnd fn = Chain (pure . fn)

data Inner a where
  Inner :: (Chainable c b) => (Traversal' a b) -> c b -> Inner a

inner :: (Chainable c b) => Traversal' a b -> c b -> Inner a
inner = Inner

inners :: (Chainable c b) => Traversal' a b -> [c b] -> Inner a
inners trav cbs = inner (partsOf trav) thing
  where
    thing = Fn (\vals -> sequenceA [after val cb | val <- vals, cb <- cbs])

key' :: (Lerp b) => Traversal' a b -> b -> Ease Float -> Time -> Inner a
key' trav end _ 0 = inner trav (mapEnd (const end))
key' trav end easing duration = inner trav (Fn (\start -> (stretch duration . ease easing) $ lerp start end))

key :: (Lerp b) => Traversal' a b -> b -> Time -> Inner a
key trav val = key' trav val cubicInOut

ky :: (Lerp a) => a -> Time -> Inner a
ky = key id

ky' :: (Lerp a) => a -> Ease Float -> Time -> Inner a
ky' = key' id

simul :: [Time -> Inner a] -> Time -> Keys a
simul keys time = Keys (map ($ time) keys)

newtype Keys a = Keys [Inner a]

keys :: [Inner a] -> Keys a
keys = Keys

instance Chainable Keys a where
  after initial (Keys keys) = foldr applyInner (pure initial) keys
    where
      applyInner (Inner trav c) = set (partsOf $ sigLens trav) x
        where
          x = map (`after` c) $ toListOf trav initial

instance Chainable Inner a where
  after initial a = after initial (Keys [a])

(|~) :: (Chainable k a) => a -> k a -> Signal a
initial |~ k = initial `after` k

(~>) :: (Chainable k1 a, Chainable k2 a) => k1 a -> k2 a -> Chain a
c1 ~> c2 = Chain (\val -> a val <> after (end (a val)) c2)
  where
    a val = after val c1

infixl 7 ~>

infixl 6 |~

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
