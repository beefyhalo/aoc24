{-# LANGUAGE ImportQualifiedPost #-}

module Day5 (module Day5) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, sepBy)
import Data.List (sortBy)
import Data.Set qualified as Set

data Rule = Before Int Int deriving (Eq, Ord, Show)

type OrderingRules = Set.Set Rule

type Update = [Int]

data Input = Input OrderingRules [Update] deriving (Show)

ordered :: OrderingRules -> Update -> Bool
ordered _ [] = True
ordered rules (x : xs) = all ((`Set.member` rules) . Before x) xs && ordered rules xs

middleSum :: Update -> Int
middleSum u = u !! (length u `div` 2)

solution :: Input -> Int
solution (Input rules updates) = sum $ map go updates
  where
    go u | ordered rules u = middleSum u
    go _ = 0

partTwo :: Input -> Int
partTwo (Input rules updates) = sum $ map go (filter (not . ordered rules) updates)
  where
    go = middleSum . sortBy (\x y -> if Before x y `Set.member` rules then LT else GT)

parser :: Parser Input
parser = do
  rules <- ruleParser `sepBy` endOfLine
  endOfLine
  endOfLine
  updates <- updateParser `sepBy` endOfLine
  pure $ Input (Set.fromList rules) updates
  where
    ruleParser = liftA2 Before (decimal <* char '|') decimal
    updateParser = decimal `sepBy` char ','
