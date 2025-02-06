{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Day12 (module Day12) where

import Control.Comonad (extend, extract)
import Control.Comonad.Representable.Store (peek, pos, seek)
import Control.Monad.Representable.Reader (tabulate)
import Data.Attoparsec.ByteString.Char8 (Parser, endOfLine, many1, satisfy, sepBy)
import Data.Foldable (fold)
import Data.List (unfoldr)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
import GHC.IO (unsafePerformIO)
import GHC.TypeLits (KnownNat)
import Grid (Coord, Grid, GridF, cardinal, diagonals, fromArray, preview, toArray)
import Search (fill)

newtype Plant = Plant Char deriving (Ord, Eq)

instance Show Plant where
  show (Plant c) = [c]

type Input n = Grid n Plant

-- | Return a list of regions in the input map.
regions :: forall n a. (KnownNat n, Eq a) => Grid n a -> [Set.Set (Coord n)]
regions g = flip unfoldr coords \input ->
  [(r, Map.withoutKeys input r) | (start, _) <- Map.lookupMin input, let r = region (seek start g)]
  where
    coords :: Map.Map (Coord n) a
    coords = fold $ tabulate @(GridF n) \c -> Map.singleton c (peek c g)

    region :: Grid n a -> Set.Set (Coord n)
    region g' =
      let start = pos g'
          label = extract g'
          step i = [j | j <- cardinal i, peek j g' == label]
       in fill step start

edgeCount :: (KnownNat n, Eq a) => Grid n a -> Int
edgeCount g =
  let start = pos g
      label = extract g
      xs = cardinal start
   in 4 - length xs + sum [1 | x <- xs, peek x g /= label]

edges :: (KnownNat n, Eq a) => Grid n a -> Grid n Int
edges = extend edgeCount

solution :: forall n. (KnownNat n) => Input n -> Int
solution g = sum $ uncurry (*) <$> rs
  where
    es = edges g
    !_ = unsafePerformIO $ putStrLn $ preview es
    rs = [(length r, sum $ (`peek` es) <$> Set.toList r) | r <- regions g]

countCorners :: forall n. KnownNat n => Set.Set (Coord n) -> Int
countCorners coords = sum [corners c | c <- Set.toList coords, isBoundary c]
  where
    isBoundary :: Coord n -> Bool
    isBoundary coord = any (`Set.notMember` coords) (cardinal coord)

    -- look at each diagonal and from there look at the cardinal points.
    -- if there are no cardinal points that are part of the region, that is a corner
    corners :: Coord n -> Int
    corners c = sum [1 | d <- diagonals c, all (`Set.notMember` coords) (cardinal d)]

partTwo :: forall n. (KnownNat n) => Input n -> Int
partTwo g = sum $ uncurry (*) <$> rs
  where
    rs = [traceShowId (length r, countCorners r) | r <- regions g]

parser :: (KnownNat n) => Parser (Input n)
parser = fromArray . toArray <$> many1 plantParser `sepBy` endOfLine
  where
    plantParser = Plant <$> satisfy (`notElem` "\r\n")

-- countCorners :: forall n. (KnownNat n) => Set.Set (Coord n) -> Int
-- countCorners region = length [c | c <- Set.toList region, isBoundary c, isCorner c]
--   where
--
--     isCorner :: Coord n -> Bool
--     isCorner coord =
--       let directNeighbors = filter (`Set.member` region) (cardinal coord)
--           diagonalNeighbors = filter (`Set.member` region) (diagonals coord)
--        in length directNeighbors <= 2 && length diagonalNeighbors < 2

-- sides :: Set.Set (Coord n) -> Int
-- sides region = length $ sides' (Set.toList region) Set.empty
--   where
--     sides' [] _ = []
--     sides' (x : xs) nope = new ++ sides' xs (S.union nope (S.fromList fs))
--       where
--         new = filter (check nope) fs
--         fs = filter (not . flip Set.member region . fst) $ zip (map (x +) [n, s, e, w]) [S, N, W, E]
--         check ns (x, S) = not $ Set.member ((x + e), S) ns || S.member ((x + w), S) ns
--         check ns (x, N) = not $ Set.member ((x + e), N) ns || S.member ((x + w), N) ns
--         check ns (x, E) = not $ Set.member ((x + n), E) ns || S.member ((x + s), E) ns
--         check ns (x, W) = not $ Set.member ((x + n), W) ns || S.member ((x + s), W) ns

-- beginning :: Set.Set (Coord n, Coord n) -> (Coord n, Coord n) -> Bool
-- beginning xs (p, d)
--   | Just d' <- p `plus` turnLeft d = (d', d) `Set.notMember` xs
--   | otherwise = False

-- -- | Find the perimeter edges of a region.
-- edges :: (KnownNat n) => Set.Set (Coord n) -> Set.Set (Coord n, Coord')
-- edges cs = Set.fromList [ (i, j - i) | i <- Set.toList cs, j <- cardinal' i, Set.notMember j cs]
