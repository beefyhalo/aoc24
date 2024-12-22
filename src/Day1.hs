{-# LANGUAGE ImportQualifiedPost #-}

module Day1 ( module Day1 ) where

import Control.Arrow ( Arrow((***)) )
import Data.List ( sort )
import Data.Monoid ( Sum(Sum, getSum) )
import Control.Applicative ()
import Data.Foldable ( Foldable(foldMap') )
import Data.Map.Monoidal qualified as Map
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 ( sepBy, decimal, skipSpace, endOfLine )


type Input = ([Int], [Int])

solution :: Input -> Int
solution = sum . map abs . uncurry (zipWith (-)) . (sort *** sort)

partTwo :: Input -> Int
partTwo (l, r) = getSum (foldMap score l)
  where
    score i = Sum (i * occurences i)
    occurences i = length $ filter (i ==) r

-- Same but O(n*log n)
partTwo' :: Input -> Int
partTwo' (l, r) = getSum (foldMap score l)
  where
    score i = Sum i * occurences i
    occurences i = Map.findWithDefault 0 i r'
    r' = foldMap' (`Map.singleton` 1) r

parser :: Parser Input
parser = unzip <$> liftA2 (,) (decimal <* skipSpace) decimal `sepBy` endOfLine
