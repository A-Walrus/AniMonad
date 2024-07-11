{-# LANGUAGE MonoLocalBinds #-}

module AniMonad.Core.Keys where

import AniMonad.Core.Lerp
import AniMonad.Core.Signal (Inner, Time, ease, inner, mapEnd, stretchBy)
import Control.Lens (Traversal')
import Ease (Ease, cubicInOut)

key' :: (Lerp b) => Traversal' a b -> b -> Ease Float -> Time -> Inner a
key' trav end _ 0 = inner trav (mapEnd (const end))
key' trav end easing duration = inner trav (\start -> (stretchBy duration . ease easing) $ lerp start end)

key :: (Lerp b) => Traversal' a b -> b -> Time -> Inner a
key trav val = key' trav val cubicInOut

ky :: (Lerp a) => a -> Time -> Inner a
ky = key id

ky' :: (Lerp a) => a -> Ease Float -> Time -> Inner a
ky' = key' id
