{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day16 (module Day16) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, choice, endOfLine, many1, sepBy)
import GHC.TypeNats (KnownNat)
import Grid (Grid, fromList')

data Cell = Empty | Wall | Start | End deriving (Eq)

instance Show Cell where
  show = \case
    Empty -> "."
    Wall -> "#"
    Start -> "S"
    End -> "E"

type Input n = Grid n Cell

-- step :: Grid n Cell -> Maybe (Coord n)

solution :: Input n -> Int
solution _ = 123

parser :: forall n. (KnownNat n) => Parser (Input n)
parser = fromList' (const (== Start)) <$> many1 cellParser `sepBy` endOfLine
  where
    cellParser = choice [Empty <$ char '.', Wall <$ char '#', Start <$ char 'S', End <$ char 'E']