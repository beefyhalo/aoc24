{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day14 (module Day14) where

import Control.Comonad (extend, extract)
import Control.Comonad.Representable.Store (peek, pos, store)
import Control.Lens.Indexed (ifind)
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, sepBy, signed, string)
import Data.Finite (finite, getFinite, modulo)
import Data.Foldable (fold, traverse_)
import Data.Functor.Rep (Representable (tabulate))
import Data.Map.Monoidal.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Vector.Sized qualified as V
import GHC.IO (unsafePerformIO)
import GHC.TypeNats (KnownNat, type (+))
import Grid (Coord' (C), Grid, Grid' (G), GridF', origin, render, unwrap)
import Linear (V2 (V2))

newtype Robot = Robot (V2 Integer) -- represents its velocity
  deriving (Show)

type Input n m = Grid' n m [Robot]

-- move that wraps
next :: (KnownNat n, KnownNat m) => Coord' n m -> Robot -> Coord' n m
next (C (getFinite -> y1, getFinite -> x1)) (Robot (V2 y2 x2)) = C (modulo $ y1 + y2, modulo $ x1 + x2)

-- Step function: move robots and update the grid
step :: forall n m. (KnownNat n, KnownNat m) => Input n m -> Input n m
step g =
  let -- Get all current robots and their new positions
      allRobots :: Map.MonoidalMap (Coord' n m) [Robot]
      allRobots = fold $ tabulate @(GridF' n m) \c ->
        Map.fromList [(newPos, [robot]) | robot <- peek c g, let newPos = next c robot]
   in extend (\g' -> fold $ Map.lookup (pos g') allRobots) g

walk :: (KnownNat n, KnownNat m) => Input n m -> [Input n m]
walk = iterate step

splitIntoQuadrants :: (KnownNat n, KnownNat m) => Grid' (n + (1 + n)) (m + (1 + m)) a -> Grid 2 (Grid' n m a)
splitIntoQuadrants (unwrap -> rows) =
  let (top, V.tail -> bottom) = V.splitAt rows
      (tlRows, trRows) = (V.splitAt <$> top, V.splitAt <$> bottom)
      (topLeft, topRight) = (fst <$> tlRows, V.tail . snd <$> tlRows)
      (bottomLeft, bottomRight) = (fst <$> trRows, V.tail . snd <$> trRows)
   in G $
        store
          ( \case
              C (0, 0) -> G $ store (\(C (y, x)) -> flip V.index x $ V.index topLeft y) origin
              C (0, 1) -> G $ store (\(C (y, x)) -> flip V.index x $ V.index topRight y) origin
              C (1, 0) -> G $ store (\(C (y, x)) -> flip V.index x $ V.index bottomLeft y) origin
              C (1, 1) -> G $ store (\(C (y, x)) -> flip V.index x $ V.index bottomRight y) origin
          )
          origin

solution ::
  forall n m.
  (KnownNat n, KnownNat m, KnownNat (n + (1 + n)), KnownNat (m + (1 + m))) =>
  Input (n + (1 + n)) (m + (1 + m)) ->
  Int
solution = product . fmap (sum . fmap length) . splitIntoQuadrants @n @m . (!! 100) . walk

-- Calculate the variance of a list of Coord' n m
varianceCoords :: (KnownNat n, KnownNat m) => Input n m -> (Double, Double)
varianceCoords g =
  let (xs, ys) = unzip [(fromIntegral y, fromIntegral x) | C (y, x) <- coords]
   in (variance xs, variance ys)
  where
    coords = fold $ extend (\g' -> [pos g' | not $ null (extract g')]) g

    -- Calculate the mean of a list of numbers
    mean :: [Double] -> Double
    mean xs = sum xs / fromIntegral (length xs)

    -- Calculate the variance of a list of numbers
    variance :: [Double] -> Double
    variance xs =
      let m = mean xs
       in mean (map (\x -> (x - m) ^ (2 :: Int)) xs)

partTwo :: forall n m. (KnownNat n, KnownNat m) => Input n m -> Int
partTwo i = c
  where
    !_ = unsafePerformIO $ traverse_ (\a -> putStr "\ESC[2J" >> putStrLn (render (const ((\x -> if x == 0 then " " else show x) . length)) a)) (take 7038 $ walk i)
    variances = varianceCoords <$> walk i
    c = fst . fromJust $ ifind (\_ (x, y) -> x < 400 && y < 400) variances

parser :: forall n m. (KnownNat n, KnownNat m) => Parser (Input n m)
parser = G . (flip store origin . flip (Map.findWithDefault [])) <$> robots
  where
    robots, robot :: Parser (Map.MonoidalMap (Coord' n m) [Robot])
    robots = fold <$> robot `sepBy` endOfLine
    robot =
      Map.singleton
        <$> liftA2 (curry C) (string "p=" *> fmap finite decimal <* char ',') (fmap finite decimal)
        <*> liftA2 (\i j -> [Robot (V2 j i)]) (string " v=" *> signed decimal <* char ',') (signed decimal)
