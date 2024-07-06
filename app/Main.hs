{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import AniMonad
import Data.List (sortOn)

main :: IO ()
main = render anim
  where
    base = [at (V2 (x * 40) 0) (Rect 30 30 white 10) | x <- [-5 .. 5]]
    anim =
      base
        |~ simul [key (ix 4 . color) blue, key (ix 7 . color) red] 0.5
        ~> inner (ix 0 . color) (ky yellow 0.5 ~> ky limegreen 0.5)
        ~> key (ixs [4, 7] . y) 40 1
        ~> inner (partsOf (ixs [4, 7] . x)) (fn (\[a, b] -> ky [b, a] 1))
        ~> key (ixs [4, 7] . y) 0 1
        ~> mapEnd (sortOn (view x))
        ~> key (ixs [4, 7, 0] . color) white 0.5
        ~> inners (traverse . y) [delay (i * 0.075) ~> ky (-50) 0.3 ~> ky 0 0.3 | i <- [0 .. 10]]
