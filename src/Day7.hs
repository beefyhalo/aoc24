{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Day7 (module Day7) where

import Data.Attoparsec.ByteString (Parser, string)
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine, sepBy, space)

type Row = (Integer, [Integer])

type Input = [Row]

isGood :: [Integer -> Integer -> Integer] -> Row -> Bool
isGood _ (_, []) = True
isGood ops (target, num : nums) = go num nums
  where
    go lhs (x : xs) | lhs <= target = any (\op -> go (lhs `op` x) xs) ops
    go lhs _ = lhs == target

solution :: Input -> Integer
solution = sum . map fst . filter (isGood [(*), (+)])

partTwo :: Input -> Integer
partTwo = sum . map fst . filter (isGood [(*), (+), cat])
  where
    cat :: Integer -> Integer -> Integer -- = read (show x ++ show y)
    cat x y = x * 10 ^ n + y
      where
        n :: Integer
        n = floor @Float (logBase 10 (fromIntegral y)) + 1

parser :: Parser Input
parser = rowParser `sepBy` endOfLine
  where
    rowParser = liftA2 (,) (decimal <* string ": ") (decimal `sepBy` space)
