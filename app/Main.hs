{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import AniMonad
import Data.Foldable (fold)
import Data.List (sortOn)
import Text.Printf

main :: IO ()
main = let ?config = Config 1024 1024 60 black in tricky

tricky :: (?config :: Config) => IO ()
tricky = render anim
  where
    base = row 20 [Rect 100 100 blue 10, Rect 100 100 red 10]
    movement =
      simul [key (ix 0 . y) 60, key (ix 1 . y) (-60)] 1
        <> simul [key (ix 0 . x) 60, key (ix 1 . x) (-60)] 1
        <> key (ixs [0, 1] . y) 0 1

    colors = simul [key (ix 0 . color) red, key (ix 1 . color) blue] 3
    anim = base |> keys [movement, colors]

simple :: (?config :: Config) => IO ()
simple = render anim
  where
    anim =
      Circle 100 white
        |> key color blue 2
        <> keyFn radius (* 2) 0.5
        <> inner radius (constSig ((* 100) . (+ 1) <$> sample (2 * pi) cos))
        <> delay 1

geometry :: (?config :: Config) => IO ()
geometry = render anim
  where
    rad = 100 |> ky 200 1 <> ky 100 1
    base = pure (Circle 0 red, Text "" 40 white)
    anim = sigSet (_2 . str) (printf "%.1f" <$> rad) . sigSet (_1 . radius) rad $ base

layout :: (?config :: Config) => IO ()
layout = render anim
  where
    r = Rect 100 100 white 10
    c = Circle 75 blue
    base = [SomeElem r, SomeElem (at (V2 0 0) r), SomeElem r, SomeElem c, SomeElem r]
    anim =
      row 20
        <$> base
          |> key (ix 0 . (as @Rect) . width) 200 1
          <> inner (ix 3 . (as @Circle) . radius) (ky 25 1 <> ky 100 1)
          <> inner (ix 1 . (as @(Transformed Rect))) (key color tomato 1 <> key rotation 90 1)

sort :: (?config :: Config) => IO ()
sort = let ?config = ?config {backgroundColor = bgColor} in render anim
  where
    itemColor = sRGB24read "#95a5a6"
    disabledColor = sRGB24read "#424949"
    bgColor = sRGB24read "#66646C"
    values = [15, 62, 30, 69, 58, 44, 81] :: [Int]
    len = length values
    try_swap a b t =
      key (ixs [a, b] . y) 50 t
        <> innerFn
          (partsOf (ixs [a, b]))
          ( \[n1, n2] ->
              if (n1 ^. _2 . str) > (n2 ^. _2 . str)
                then
                  innerFn (partsOf (traverse . x)) (\[a1, a2] -> ky [a2, a1] t)
                else
                  mempty
          )
        <> key (ixs [a, b] . y) 0 t
        <> mapEnd (sortOn (view x))
    times = 0.5 : map (max 0.2 . (* 0.9)) times
    sort = fold (zipWith sortPass [(len - 2), (len - 3) .. (-1)] times)
    sortPass i t = foldMap (\b -> try_swap b (b + 1) t) [0 .. i] <> key (ix (i + 1) . _1 . color) disabledColor t
    base = row 20 [(Rect 120 120 itemColor 24, Text val 40 white) | val <- values]
    anim =
      base
        |> sort
        <> delay 0.2
        <> key (traverse . _1 . color) itemColor 0.6
        <> inners (traverse . y) [delay (i * 0.05) <> ky (-90) 0.2 <> ky 0 0.2 | i <- [0 ..]]
        <> delay 1
