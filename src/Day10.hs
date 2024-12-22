{-# LANGUAGE ScopedTypeVariables #-}

module Day10 (module Day10) where

import Control.Applicative ((<|>))
import Control.Comonad (extend, extract)
import Control.Comonad.Representable.Store (ComonadStore (seek))
import Control.Comonad.Store (pos)
import Control.Monad.Loops (iterateUntilM)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 (char, digit, endOfLine, many1, sepBy)
import Data.Char (digitToInt)
import qualified Data.Set as Set
import GHC.TypeLits (KnownNat)
import Grid (Coord, Grid, cardinal, fromArray, toArray)

data Pos = H Int | Unreachable deriving (Eq)

instance Show Pos where
  show (H h) = show h
  show Unreachable = "."

type Input n = Grid n Pos

solution :: forall n. (KnownNat n) => Input n -> Int
solution = score . positions . trails
  where
    positions :: Grid n [Grid n Pos] -> Grid n (Set.Set (Coord n))
    positions = fmap (Set.fromList . fmap pos)

    score :: Grid n (Set.Set (Coord n)) -> Int
    score = sum . fmap length

partTwo :: (KnownNat n) => Input n -> Int
partTwo = sum . positions . trails
  where
    positions :: Grid n [Grid n Pos] -> Grid n Int
    positions = fmap length

trails :: (KnownNat n) => Grid n Pos -> Grid n [Grid n Pos]
trails = extend trail

trail :: (KnownNat n) => Grid n Pos -> [Grid n Pos]
trail g
  | extract g == H 0 = iterateUntilM ((== H 9) . extract) step g
  | otherwise = []

step :: (KnownNat n) => Grid n Pos -> [Grid n Pos]
step g
  | H h <- extract g = [next | c <- cardinal (pos g), let next = seek c g, isNext h next]
  | otherwise = []
  where
    isNext h g'
      | H h' <- extract g' = h + 1 == h'
      | otherwise = False

parser :: (KnownNat n) => Parser (Input n)
parser = fromArray . toArray <$> many1 hParser `sepBy` endOfLine
  where
    hParser = fmap (H . digitToInt) digit <|> (Unreachable <$ char '.')