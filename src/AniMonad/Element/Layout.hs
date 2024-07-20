{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AniMonad.Element.Layout where

import AniMonad.Element.Base

row :: (Element e) => Float -> [e] -> [Transformed e]
row gap elements = zipWith move starts elements
  where
    num_gaps = length elements - 1
    widths = map (boxWidth . box) elements
    total_width = (fromIntegral num_gaps * gap) + sum widths
    start = -(total_width / 2)
    starts = scanl (\x w -> x + w + gap) start widths
    move x e = at (V2 (x - elem_x) 0) e
      where
        (BoundingBox (V2 elem_x _) _) = box e

column :: (Element e) => Float -> [e] -> [Transformed e]
column gap elements = zipWith move starts elements
  where
    num_gaps = length elements - 1
    heights = map (boxHeight . box) elements
    total_height = (fromIntegral num_gaps * gap) + sum heights
    start = -(total_height / 2)
    starts = scanl (\y h -> y + h + gap) start heights
    move y e = at (V2 0 (y - elem_y)) e
      where
        (BoundingBox (V2 _ elem_y) _) = box e
