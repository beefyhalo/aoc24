{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Day15 (module Day15) where

import Control.Applicative (optional)
import Control.Arrow ((***))
import Control.Comonad (extract, (<<=))
import Control.Comonad.Representable.Store (peek, pos, store)
import Data.Array.IArray (Array)
import Data.Attoparsec.ByteString.Char8 (Parser, char, choice, endOfLine, many1, sepBy)
import Data.Coerce (coerce)
import Data.Finite (finite, getFinite, weakenN)
import Data.Foldable (foldl')
import Data.Foldable.WithIndex (ifind)
import Data.Functor.Compose (getCompose)
import Data.Maybe (fromJust)
import Data.Vector.Sized qualified as V
import Debug.Trace (traceShow, traceShowId)
import GHC.TypeNats (KnownNat)
import GHC.TypeNats qualified
import Grid (Coord, Coord', Grid, Grid' (G), east, fromArray', move', north, plus', south, toArray, unGrid, unwrap, update, west)
import Linear (V2)

data Space = Empty | Box | BoxLeft | BoxRight | Wall | Player deriving (Eq)

instance Show Space where
  show = \case
    Empty -> "."
    Box -> "O"
    BoxLeft -> "["
    BoxRight -> "]"
    Wall -> "#"
    Player -> "@"

type Input n = (Grid n Space, [V2 Integer])

step :: (KnownNat n, KnownNat m) => Grid' n m Space -> V2 Integer -> Grid' n m Space
step grid dir = traceShowId $ case move' grid dir of
  Just nextGrid
    | extract nextGrid == Empty -> movePlayer
    | Just nextSpace <- findNextFreeSpace dir (pos nextGrid) nextGrid ->
        update nextSpace Box movePlayer
    where
      movePlayer = update (pos grid) Empty $ update (pos nextGrid) Player nextGrid
  _ -> grid

-- Find the next free space in the direction of the push
findNextFreeSpace :: (KnownNat n, KnownNat m) => V2 Integer -> Coord' n m -> Grid' n m Space -> Maybe (Coord' n m)
findNextFreeSpace dir i grid =
  case peek i grid of
    Empty -> Just i -- Found free space
    Box | Just nextPos <- i `plus'` dir -> findNextFreeSpace dir nextPos grid -- Keep going
    _ -> Nothing -- Obstruction (Wall or out of bounds)

solution :: (KnownNat n) => Input n -> Int
solution (g, dirs) = sum $ score <<= foldl' step g dirs

score :: (KnownNat n, KnownNat m) => Grid' n m Space -> Int
score g = case extract g of
  Box -> let (fromIntegral -> y, fromIntegral -> x) = pos g in 100 * y + x
  _ -> 0

partTwo :: (KnownNat n, KnownNat (n GHC.TypeNats.* 2)) => Input n -> Int
partTwo (g, dirs) = length $ foldl' step (expand g) dirs

expand :: (KnownNat n, KnownNat (n GHC.TypeNats.* 2)) => Grid n Space -> Grid' n (n GHC.TypeNats.* 2) Space
expand g =
  let v = fmap (V.concatMap expandCell) (getCompose . unwrap $ unGrid g)
   in G $ store (uncurry (V.index . (v `V.index`))) start
  where
    start = let (y, getFinite -> x) = pos g in (y, finite $ x * 2)
    expandCell :: Space -> V.Vector 2 Space
    expandCell = \case
      Empty -> V.fromTuple (Empty, Empty)
      Player -> V.fromTuple (Player, Empty)
      Box -> V.fromTuple (BoxLeft, BoxRight)
      Wall -> V.fromTuple (Wall, Wall)
      _ -> undefined

-- TODO: Use fromMap sparse map representation
parser :: forall n. (KnownNat n) => Parser (Input n)
parser = liftA2 (,) (parser1 <* endOfLine <* endOfLine) parser2
  where
    parser1 :: Parser (Grid n Space)
    parser1 = uncurry fromArray' . toInput . toArray <$> many1 spaceParser `sepBy` endOfLine
      where
        spaceParser = choice [Empty <$ char '.', Box <$ char 'O', Wall <$ char '#', Player <$ char '@']

        toInput :: Array (Int, Int) Space -> (Array (Int, Int) Space, Coord n)
        toInput arr = (arr, fromIntegral *** fromIntegral $ playerCoord)
          where
            playerCoord = fst . fromJust $ ifind (const (== Player)) arr

    parser2 :: Parser [V2 Integer]
    parser2 = many1 $ choice [north <$ char '^', east <$ char '>', west <$ char '<', south <$ char 'v'] <* optional endOfLine
