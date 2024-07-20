
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
