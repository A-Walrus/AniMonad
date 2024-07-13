{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import AniMonad
import Data.Foldable (fold)
import Data.List (sortOn)

itemColor :: Color
itemColor = sRGB24read "#95a5a6"

disabledColor :: Color
disabledColor = sRGB24read "#424949"

main :: IO ()
main = render ((,) background <$> anim)
  where
    background = Rect 1024 1024 (sRGB24read "#66646C") 0
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
      base
        |> sort
        <> delay 0.2
        <> key (traverse . _1 . color) itemColor 0.6
        <> inners (traverse . y) [delay (i * 0.05) <> ky (-90) 0.2 <> ky 0 0.2 | i <- [0 .. 10]]
        <> delay 1
