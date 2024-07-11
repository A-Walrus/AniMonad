{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import AniMonad
import Data.List (sortOn)

main :: IO ()
main = render anim
  where
    count = 10 :: Int
    base = [at (V2 ((fromIntegral x - (fromIntegral count / 2)) * 80) 0) (Rect 60 60 white 10, Text (show x) 30 black) | x <- [0 .. (count - 1)]]
    anim =
      base |> simul [key (ix 4 . _1 . color) blue, key (ix 7 . _1 . color) red] 0.5
        <> inner (ix 0 . _2 . color) (ky yellow 0.5 <> ky limegreen 0.5)
        <> key (ix 0 . rotation) 90 1
        <> key (ixs [4, 7] . y) (-80) 1
        <> inner (partsOf (ixs [4, 7] . x)) (\[a, b] -> ky [b, a] 1)
        <> key (ixs [4, 7] . y) 0 1
        <> mapEnd (sortOn (view x))
        <> key (traverse . _1 . color) white 0.5
        <> inners (traverse . y) [delay (i * 0.05) <> ky (-90) 0.2 <> ky 0 0.2 | i <- [0 .. 10]]
