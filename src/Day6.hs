{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Day6 (module Day6) where

import Control.Arrow (Arrow ((&&&)), (***))
import Control.Comonad.Store (ComonadStore (..))
import Control.Monad (when)
import Control.Monad.Loops (unfoldrM)
import Control.Monad.State.Strict (MonadState (put), State, evalState, get, gets, modify, runState)
import Control.Parallel.Strategies (parList, rpar, using)
import Data.Array.IArray (Array)
import Data.Attoparsec.ByteString (Parser, choice, many1)
import Data.Attoparsec.ByteString.Char8 (char, endOfLine, sepBy)
import Data.Foldable (Foldable (foldMap'))
import Data.Foldable.WithIndex (ifind)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import GHC.IO (unsafePerformIO)
import GHC.TypeLits (KnownNat)
import Grid (Coord, Grid, fromArray', move, north, render, toArray, turnRight, update, (!?))
import Linear (V2)

data Space = Empty | Obstacle | Guard | Circle deriving (Eq)

instance Show Space where
  show = \case
    Empty -> "."
    Obstacle -> "#"
    Circle -> "O"
    Guard -> "^"

type Input n = Grid n Space

step :: forall n. (KnownNat n) => Grid n Space -> State (V2 Integer) (Maybe (Grid n Space))
step g = do
  space <- gets (g !?)
  when (space == Just Obstacle || space == Just Circle) $ modify turnRight
  gets nextGrid
  where
    -- Instead of just moving one step, keep moving in the same direction until we hit a boulder
    -- TODO this can prob be whileJust/unfoldrM'
    nextGrid :: V2 Integer -> Maybe (Grid n Space)
    nextGrid d = case g !? d of
      space | space == Just Obstacle || space == Just Circle -> Just g
      Just _ -> move g d
      _ -> Nothing

walk :: forall n. (KnownNat n) => Grid n Space -> Set.Set (Coord n)
walk g = withGuard $ evalState (Set.fromList <$> path) north
  where
    withGuard = Set.insert (pos g)

    path :: State (V2 Integer) [Coord n]
    path = unfoldrM (fmap (fmap (pos &&& id)) . step) g

solution :: forall n. (KnownNat n) => Input n -> Int
solution g = length visited
  where
    visited = walk g
    !_ = unsafePerformIO $ putStrLn $ "\n" ++ render (\i s -> if Set.member i visited then "X" else show s) g

partTwo :: forall n. (KnownNat n) => Input n -> Int
partTwo = length . (`using` parList rpar) . filter detectLoop . withObstacles
  where
    withObstacles :: Grid n Space -> [Grid n Space]
    withObstacles = placeObstacles <*> walk

    placeObstacles :: Grid n Space -> Set.Set (Coord n) -> [Grid n Space]
    placeObstacles g = foldMap' (pure . place)
      where
        place :: Coord n -> Grid n Space
        place c = update c Circle g

    detectLoop :: Grid n Space -> Bool
    detectLoop = flip evalState (north, Set.empty) . go
      where
        go g = do
          (dir, visited) <- get
          let currentSpace = (pos g, dir)
          if
            | currentSpace `Set.member` visited ->
                -- let !_ = unsafePerformIO $ putStrLn (render (renderSpace visited) g ++ "\n") in
                pure True
            | (Just nextStep, nextDir) <- runState (step g) dir ->
                put (nextDir, Set.insert currentSpace visited) >> go nextStep
            | otherwise -> pure False

-- renderSpace v i s
--   | inLR && inUD = "+"
--   | inLR = "-"
--   | inUD = "|"
--   | otherwise = show s
--   where
--     inLR = not $ Set.fromList [(i, east), (i, west)] `Set.disjoint` v
--     inUD = not $ Set.fromList [(i, north), (i, south)] `Set.disjoint` v

parser :: forall n. (KnownNat n) => Parser (Input n)
parser = uncurry fromArray' . toInput . toArray <$> many1 spaceParser `sepBy` endOfLine
  where
    spaceParser = choice [Empty <$ char '.', Obstacle <$ char '#', Guard <$ char '^']

    toInput :: Array (Int, Int) Space -> (Array (Int, Int) Space, Coord n)
    toInput arr = (arr, fromIntegral *** fromIntegral $ guardCoord)
      where
        guardCoord = fst . fromJust $ ifind (const (== Guard)) arr
