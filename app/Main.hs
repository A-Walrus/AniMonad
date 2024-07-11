{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import AniMonad
import Data.Foldable (fold)
import Data.List (sortOn)

main :: IO ()
main = render anim
  where
    values = [5, 4, 7, 6, 2, 3, 1] :: [Int]
    try_swap :: Int -> Int -> Time -> Chain [Transformed (Rect, Text Int)]
    try_swap a b t =
      key (ixs [a, b] . y) (-80) t
        <> innerFn
          (partsOf (ixs [a, b]))
          ( \[n1, n2] ->
              if (n1 ^. _2 . str) > (n2 ^. _2 . str)
                then
                  inner (partsOf (traverse . x)) (\[a1, a2] -> ky [a2, a1] t)
                else
                  mempty
          )
        <> key (ixs [a, b] . y) 0 t
        <> mapEnd (sortOn (view x))
    times = 0.5 : map (max 0.2 . (* 0.9)) times
    sort = fold (zipWith ($) [try_swap b (b + 1) | a <- [(length values - 2), (length values - 3) .. 0], b <- [0 .. a]] times)
    base = zipWith (\i v -> at (V2 ((i - (fromIntegral (length values) / 2)) * 80) 0) (Rect 60 60 white 10, Text v 30 black)) [0 ..] values
    anim =
      base
        |> sort
        <> inners (traverse . y) [delay (i * 0.05) <> ky (-90) 0.2 <> ky 0 0.2 | i <- [0 .. 10]]
