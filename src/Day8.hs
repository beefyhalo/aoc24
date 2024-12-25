{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day8 (module Day8) where

import Data.Attoparsec.ByteString (Parser, choice, many1)
import Data.Attoparsec.ByteString.Char8 (char, endOfLine, satisfy, sepBy)
import Data.Foldable.WithIndex (FoldableWithIndex (ifoldMap))
import Data.Map.Monoidal qualified as Map
import Data.Set qualified as Set
import GHC.IO (unsafePerformIO)
import GHC.TypeLits (KnownNat)
import Grid (Coord, Grid, colines, fromArray, modify, render, toArray)

data Space = Empty | Antenna Char | Antinode deriving (Eq, Ord)

instance Show Space where
  show = \case
    Empty -> "."
    Antenna a -> [a]
    Antinode -> "#"

type Input n = Grid n Space

antinodes :: forall n. (KnownNat n) => Grid n Space -> [[Coord n]]
antinodes g = colines <$> antennae
  where
    antennae :: [[Coord n]]
    antennae = Map.elems $ flip ifoldMap g \c -> \case
      a@(Antenna _) -> Map.singleton a [c]
      _ -> mempty

placeAntinodes :: Set.Set (Coord n) -> Grid n Space -> Grid n Space
placeAntinodes locs = modify (`Set.member` locs) Antinode

solution :: (KnownNat n) => Input n -> Int
solution = length . gather . antinodes
  where
    gather = foldMap (foldMap Set.singleton)

partTwo :: (KnownNat n) => Input n -> Int
partTwo g = length locs
  where
    gather = foldMap Set.fromList
    locs = gather (antinodes g)
    !_ = unsafePerformIO $ putStrLn $ "\n" ++ render (const show) (placeAntinodes locs g)

parser :: (KnownNat n) => Parser (Input n)
parser = fromArray . toArray <$> many1 spaceParser `sepBy` endOfLine
  where
    spaceParser = choice [Empty <$ char '.', Antenna <$> satisfy (`notElem` "\r\n")]
