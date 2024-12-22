{-# LANGUAGE OverloadedStrings #-}

module Day13 (module Day13) where

import Control.Applicative (optional)
import Data.Attoparsec.ByteString.Char8 (Parser, decimal, endOfLine, sepBy, string)
import Data.Maybe (fromMaybe)

data Machine = M (Int, Int) (Int, Int) (Int, Int)
  deriving (Show)

type Input = [Machine]

{-
Cramer's Rule for the system:

( 𝑎 𝑏 ) (𝑥) = (p)
( 𝑐 𝑑 ) (𝑦) = (q)

The solutions for 𝑥 and 𝑦 are given by:

𝑥 = det(𝐴𝑥)/det(𝐴)

and

𝑦 = det(𝐴𝑦)/det(𝐴)

where

det(𝐴) = 𝑎𝑑−𝑏𝑐

-}
solve :: Machine -> Maybe Int
solve (M (ax, ay) (bx, by) (px, py))
  | -- Check if the numerator (px * ay - py * ax) is divisible by the determinant of the coefficient matrix (det(A) = bx * ay - by * ax)
    (m, 0) <- (px * ay - py * ax) `quotRem` (bx * ay - by * ax),
    -- Solve for the second variable (n)
    (n, 0) <- (px * ay - m * bx * ay) `quotRem` (ax * ay) =
      Just (3 * n + m)
  | otherwise = Nothing

solution :: Input -> Int
solution = sum . map (fromMaybe 0 . solve)

partTwo :: Input -> Int
partTwo = solution . adjust
  where
    adjust = map (\(M a b (px, py)) -> M a b (px + 10000000000000, py + 10000000000000))

parser :: Parser Input
parser = machineParser `sepBy` endOfLine
  where
    machineParser :: Parser Machine
    machineParser =
      M
        <$> liftA2 (,) (string "Button A: X+" *> decimal) (string ", Y+" *> decimal <* endOfLine)
        <*> liftA2 (,) (string "Button B: X+" *> decimal) (string ", Y+" *> decimal <* endOfLine)
        <*> liftA2 (,) (string "Prize: X=" *> decimal) (string ", Y=" *> decimal <* optional endOfLine)
