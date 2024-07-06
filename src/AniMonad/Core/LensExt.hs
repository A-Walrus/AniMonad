module AniMonad.Core.LensExt (get, ixs, adjoin) where

import Control.Exception (assert)
import Control.Lens (Getting, Index, IxValue, Ixed, Traversal', ignored, ix)
import Control.Lens.Combinators (preview)
import Control.Lens.Unsound (adjoin)
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Monoid qualified

get :: Getting (Data.Monoid.First a) s a -> s -> a
get a b = fromJust (preview a b)

ixs :: (Ixed m, Eq (Index m)) => [Index m] -> Traversal' m (IxValue m)
ixs [] = ignored
ixs [a] = ix a
ixs l@(a : rest) = assert unique (adjoin (ix a) (ixs rest))
  where
    unique = length l == length (nub l)
