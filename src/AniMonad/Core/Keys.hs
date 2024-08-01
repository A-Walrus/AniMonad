module AniMonad.Core.Keys where

import AniMonad.Config
import AniMonad.Core.Lerp
import AniMonad.Core.Signal (Chain, Time, chain, inner, innerFn, mapEnd, sample)
import Control.Lens (Traversal')
import Ease (Ease, cubicInOut)

key' :: (?config :: Config, Lerp b) => Traversal' a b -> b -> Ease Float -> Time -> Chain a
key' trav end _ 0 = inner trav (mapEnd (const end))
key' trav end easing time = inner trav (chain (\start -> sample time (lerp start end . easing . (/ time))))

key :: (?config :: Config, Lerp b) => Traversal' a b -> b -> Time -> Chain a
key trav val = key' trav val cubicInOut

ky' :: (?config :: Config, Lerp a) => a -> Ease Float -> Time -> Chain a
ky' = key' id

ky :: (?config :: Config, Lerp a) => a -> Time -> Chain a
ky = key id

keyFn' :: (?config :: Config, Lerp b) => Traversal' a b -> (b -> b) -> Ease Float -> Time -> Chain a
keyFn' trav fn easing time = innerFn trav (\v -> ky' (fn v) easing time)

keyFn :: (?config :: Config, Lerp b) => Traversal' a b -> (b -> b) -> Time -> Chain a
keyFn trav f = keyFn' trav f cubicInOut

kyFn' :: (?config :: Config, Lerp a) => (a -> a) -> Ease Float -> Time -> Chain a
kyFn' = keyFn' id

kyFn :: (?config :: Config, Lerp a) => (a -> a) -> Time -> Chain a
kyFn = keyFn id
