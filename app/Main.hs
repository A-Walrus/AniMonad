{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import AniMonad
import Data.Foldable (fold)
import Data.List (sortOn)

itemColor = sRGB24read "#95a5a6"

disabledColor = sRGB24read "#424949"

bgColor = sRGB24read "#66646C"

main :: IO ()
main = sort

simple :: IO ()
simple = render anim
  where
    anim =
      Circle 100 white
        |> key color blue 2
        <> key radius 200 0.5
        <> inner radius (signal ((* 100) . (+ 1) <$> sample (2 * pi) cos))
        <> delay 1

capabilities :: IO ()
capabilities = render anim
  where
    base = [at (V2 (x * 80) 0) (Rect 60 60 white 10) | x <- [-5 .. 5]]
    anim =
      base
        |> simul [key (ix 4 . color) blue, key (ix 7 . color) red] 0.5
        <> inner (ix 0 . color) (ky yellow 0.5 <> ky limegreen 0.5)
        <> key (ixs [4, 7] . y) (-80) 1
        <> inner (partsOf (ixs [4, 7] . x)) (fn (\[a, b] -> ky [b, a] 1))
        <> key (ixs [4, 7] . y) 0 1
        <> mapEnd (sortOn (view x))
        <> key (traverse . color) white 0.5
        <> inners (traverse . y) [delay (i * 0.05) <> ky (-90) 0.2 <> ky 0 0.2 | i <- [0 .. 10]]

sort :: IO ()
sort = render anim
  where
    background = Rect 1024 1024 bgColor 0
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
    base = zipWith (\i v -> at (V2 ((i - (fromIntegral (length values - 1) / 2)) * 140) 0) (Rect 120 120 itemColor 25, Text v 40 white)) [0 ..] values
    anim =
      (,) background
        <$> base
          |> sort
          <> delay 0.2
          <> key (traverse . _1 . color) itemColor 0.6
          <> inners (traverse . y) [delay (i * 0.05) <> ky (-90) 0.2 <> ky 0 0.2 | i <- [0 .. 10]]
          <> delay 1
