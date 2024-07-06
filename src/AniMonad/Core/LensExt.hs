module AniMonad.Core.LensExt (get, ixs, adjoin) where


import Control.Lens (Ixed, Index,Traversal',IxValue,ignored,ix, Getting)
import Control.Lens.Unsound (adjoin)
import Data.List (nub)
import Control.Exception (assert)
import qualified Data.Monoid
import Data.Maybe (fromJust)
import Control.Lens.Combinators (preview)

get :: Getting (Data.Monoid.First a) s a -> s -> a
get a b = fromJust (preview a b)

ixs :: (Ixed m, Eq (Index m)) => [Index m] -> Traversal' m (IxValue m)
ixs [] = ignored
ixs [a] = ix a
ixs l@(a : rest) = assert unique (adjoin (ix a) (ixs rest))
  where
    unique = length l == length (nub l)
