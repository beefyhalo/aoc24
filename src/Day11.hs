{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

module Day11 (module Day11) where

import Data.Attoparsec.ByteString (Parser, sepBy)
import Data.Attoparsec.ByteString.Char8 (decimal, space)
import Data.MemoTrie (memo2)

type Input = [Int]

solution :: Input -> Int
solution = sum . map (blink 75)

blink :: Int -> Int -> Int
blink = memo2 \steps stone ->
  if
    | steps == 0 -> 1
    | stone == 0 -> blink (steps - 1) 1
    | (w, 0) <- length (show stone) `quotRem` 2,
      (l, r) <- stone `quotRem` (10 ^ w) ->
        blink (steps - 1) l + blink (steps - 1) r
    | otherwise -> blink (steps - 1) (2024 * stone)

parser :: Parser Input
parser = decimal `sepBy` space