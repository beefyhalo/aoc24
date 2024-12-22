{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Day3 (module Day3) where

import Control.Applicative (Alternative (many))
import Control.Monad.State.Strict (MonadState (put), evalState, gets)
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, choice, decimal, string, try)
import Data.Bool (bool)
import Data.Maybe (catMaybes)

type Input = [Instruction]

data Instruction = Mul Int Int | Do | Dont
  deriving (Show)

solution :: Input -> Int
solution = sum . map go
  where
    go (Mul a b) = a * b
    go _ = 0

partTwo :: Input -> Int
partTwo = sum . flip evalState True . traverse go
  where
    go (Mul a b) = gets (bool 0 (a * b))
    go Do = 0 <$ put True
    go Dont = 0 <$ put False

parser :: Parser Input
parser = fmap catMaybes . many $ choice [Just <$> try instrParser, Nothing <$ anyChar]
  where
    instrParser :: Parser Instruction
    instrParser =
      choice
        [ string "mul(" *> (Mul <$> decimal <* char ',' <*> decimal) <* char ')',
          Do <$ string "do()",
          Dont <$ string "don't()"
        ]