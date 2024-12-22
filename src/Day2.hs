module Day2 (module Day2) where

import Data.Attoparsec.ByteString.Char8 (Parser, decimal, endOfLine, sepBy, space)
import Data.List (inits, tails)

type Input = [Report]

type Report = [Level]

type Level = Integer

safe :: Report -> Bool
safe r = cond1 && cond2
  where
    neighbors = zipWith (-) r (tail r)
    cond1 = all (> 0) neighbors || all (< 0) neighbors
    cond2 = all (flip elem [1 .. 3] . abs) neighbors

solution :: Input -> Int
solution = length . filter safe

partTwo :: Input -> Int
partTwo = length . filter (any safe . alter)
  where
    alter r = r : zipWith (++) (inits r) (tail $ tails r)

parser :: Parser Input
parser = decimal `sepBy` space `sepBy` endOfLine
