module AniMonad.Core.Keys where

import AniMonad.Config
import AniMonad.Core.Lerp
import AniMonad.Core.Signal (Chain, Time, chain, inner, mapEnd, sample)
import Control.Lens (Traversal')
import Ease (Ease, cubicInOut)

key' :: (?config :: Config, Lerp b) => Traversal' a b -> b -> Ease Float -> Time -> Chain a
key' trav end _ 0 = inner trav (mapEnd (const end))
key' trav end easing time = inner trav (chain (\start -> sample time (lerp start end . easing . (/ time))))

key :: (?config :: Config, Lerp b) => Traversal' a b -> b -> Time -> Chain a
key trav val = key' trav val cubicInOut

ky :: (?config :: Config, Lerp a) => a -> Time -> Chain a
ky = key id

ky' :: (?config :: Config, Lerp a) => a -> Ease Float -> Time -> Chain a
ky' = key' id

keyR' :: (?config :: Config, Lerp b, Offset b) => Traversal' a b -> b -> Ease Float -> Time -> Chain a
keyR' trav offset' _ 0 = inner trav (chain (\start -> pure (offset start offset')))
keyR' trav offset' easing time = inner trav (chain (\start -> sample time (lerp start (offset start offset') . easing . (/ time))))

keyR :: (?config :: Config, Lerp b, Offset b) => Traversal' a b -> b -> Time -> Chain a
keyR trav offset' = keyR' trav offset' cubicInOut

kyR' :: (?config :: Config, Lerp a, Offset a) => a -> Ease Float -> Time -> Chain a
kyR' = keyR' id

kyR :: (?config :: Config, Lerp a, Offset a) => a -> Time -> Chain a
kyR = keyR id