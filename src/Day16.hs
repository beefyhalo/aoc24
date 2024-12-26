{-# LANGUAGE ScopedTypeVariables #-}

module Day16 (module Day16) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, choice, endOfLine, many1, sepBy)
import GHC.TypeNats (KnownNat)
import Grid (Grid', fromList')

data Cell = Empty | Wall | Start | End deriving (Eq, Show)

type Input n m = Grid' n m Cell

parser :: forall n m. (KnownNat n, KnownNat m) => Parser (Input n m)
parser = fromList' (const (== Start)) <$> many1 cellParser `sepBy` endOfLine
  where
    cellParser = choice [Empty <$ char '.', Wall <$ char '#', Start <$ char 'S', End <$ char 'E']