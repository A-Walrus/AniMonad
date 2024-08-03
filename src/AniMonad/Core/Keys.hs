module AniMonad.Core.Keys where

import AniMonad.Config
import AniMonad.Core.Lerp
import AniMonad.Core.Signal (Action, Time, fn, inner, innerFn, mapEnd, sample, signal)
import Control.Lens (Traversal')
import Ease (Ease, cubicInOut)

key' :: (?config :: Config, Lerp b) => Traversal' a b -> b -> Ease Float -> Time -> Action a
key' trav end _ 0 = inner trav (mapEnd (const end))
key' trav end easing time = inner trav (fn (signal . new_values))
  where
    new_values start = sample time (lerp start end . easing . (/ time))

key :: (?config :: Config, Lerp b) => Traversal' a b -> b -> Time -> Action a
key trav val = key' trav val cubicInOut

ky' :: (?config :: Config, Lerp a) => a -> Ease Float -> Time -> Action a
ky' = key' id

ky :: (?config :: Config, Lerp a) => a -> Time -> Action a
ky = key id

keyFn' :: (?config :: Config, Lerp b) => Traversal' a b -> (b -> b) -> Ease Float -> Time -> Action a
keyFn' trav f easing time = innerFn trav (\v -> ky' (f v) easing time)

keyFn :: (?config :: Config, Lerp b) => Traversal' a b -> (b -> b) -> Time -> Action a
keyFn trav f = keyFn' trav f cubicInOut

kyFn' :: (?config :: Config, Lerp a) => (a -> a) -> Ease Float -> Time -> Action a
kyFn' = keyFn' id

kyFn :: (?config :: Config, Lerp a) => (a -> a) -> Time -> Action a
kyFn = keyFn id
