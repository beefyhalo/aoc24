{-# LANGUAGE ImportQualifiedPost #-}

module Search (module Search) where

import Data.Foldable (foldl')
import Data.Set qualified as Set

-- | Generate a set of all the values reachable from a starting
-- state and a step function.
fill :: (Ord a) => (a -> [a]) -> a -> Set.Set a
fill f x = fillN f [x]
{-# INLINE fill #-}

-- | Generate a set of all the values reachable from a list
-- of starting states and a step function.
fillN :: (Ord a) => (a -> [a]) -> [a] -> Set.Set a
fillN f = foldl' go Set.empty
  where
    go seen x
      | x `Set.member` seen = seen
      | otherwise = foldl' go (Set.insert x seen) (f x)
{-# INLINE fillN #-}