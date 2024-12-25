{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day4 (module Day4) where

import Control.Comonad.Identity (Comonad (extend, extract))
import Control.Comonad.Representable.Store (ComonadStore (experiment))
import Data.Attoparsec.ByteString (Parser, choice, many1, sepBy)
import Data.Attoparsec.ByteString.Char8 (char, endOfLine)
import Data.Bool (bool)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Product (Product (..))
import Data.Maybe (mapMaybe)
import GHC.TypeLits (KnownNat)
import Grid (Coord, Grid, above, below, fromArray, left, plus, right, toArray)
import Linear.V2 (V2 (V2))

data Cell = X | M | A | S
  deriving (Eq, Show)

data Three a = Three a a a
  deriving (Eq, Functor, Show, Foldable, Traversable)

type Context a = Compose [] Three a

type Input n = Grid n Cell

solution :: forall n. (KnownNat n) => Input n -> Int
solution = sum . fmap occurences . applyContext
  where
    applyContext :: Grid n Cell -> Grid n (Context Cell)
    applyContext = extend (\s -> if extract s == X then experiment context s else mempty)
      where
        context :: Coord n -> Context (Coord n)
        context i =
          Compose $
            mapMaybe
              sequence
              [ Three (i `plus` j) (plus i (j * 2)) (plus i (j * 3))
                | dy <- [-1 .. 1],
                  dx <- [-1 .. 1],
                  (dy, dx) /= (0, 0),
                  let j = V2 dy dx
              ]

    occurences :: Context Cell -> Int
    occurences = length . filter (== Three M A S) . getCompose

data Two a = Two a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Context' a = Compose Maybe (Product Two Two) a

partTwo :: forall n. (KnownNat n) => Input n -> Int
partTwo = sum . fmap (bool 0 1 . isXmas) . applyContext
  where
    applyContext :: Grid n Cell -> Grid n (Context' Cell)
    applyContext = extend (\s -> if extract s == A then experiment context s else Compose Nothing)
      where
        context :: Coord n -> Context' (Coord n)
        context i =
          Compose $
            sequence $
              Pair (Two (below =<< left i) (above =<< right i)) (Two (above =<< left i) (below =<< right i))

    isXmas :: Context' Cell -> Bool
    isXmas (Compose (Just (Pair l r))) = let isMS = (`elem` [Two M S, Two S M]) in isMS l && isMS r
    isXmas _ = False

parser :: (KnownNat n) => Parser (Input n)
parser = fromArray . toArray <$> many1 cParser `sepBy` endOfLine
  where
    cParser = choice [X <$ char 'X', M <$ char 'M', A <$ char 'A', S <$ char 'S']
