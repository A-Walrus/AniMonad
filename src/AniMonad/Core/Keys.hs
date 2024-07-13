{-# LANGUAGE MonoLocalBinds #-}

module AniMonad.Core.Keys where

import AniMonad.Core.Lerp
import AniMonad.Core.Signal (Chain, Time, chain, inner, mapEnd, sample)
import Control.Lens (Traversal')
import Ease (Ease, cubicInOut)

key' :: (Lerp b) => Traversal' a b -> b -> Ease Float -> Time -> Chain a
key' trav end _ 0 = inner trav (mapEnd (const end))
key' trav end easing time = inner trav (chain (\start -> sample time (lerp start end . easing . (/ time))))

key :: (Lerp b) => Traversal' a b -> b -> Time -> Chain a
key trav val = key' trav val cubicInOut

ky :: (Lerp a) => a -> Time -> Chain a
ky = key id

ky' :: (Lerp a) => a -> Ease Float -> Time -> Chain a
ky' = key' id
