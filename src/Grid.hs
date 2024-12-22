{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Grid (module Grid) where

import Control.Arrow ((***))
import Control.Comonad (Comonad, duplicate, extend, extract)
import Control.Comonad.Identity (Identity (Identity))
import Control.Comonad.Representable.Store (ComonadStore, Store, StoreT (StoreT), pos, runStore, seek, store)
import Control.Lens (from, (&), (.~))
import Data.Array.IArray (Array, array, (!))
import Data.Bitraversable (bitraverse)
import Data.Coerce (coerce)
import Data.Distributive (Distributive (..))
import Data.Finite (Finite, getFinite, packFinite)
import Data.Functor.Compose (Compose (getCompose))
import Data.Functor.Rep (Representable (..), distributeRep, tabulated)
import Data.List (unfoldr)
import Data.Maybe (catMaybes)
import Data.Vector.Sized qualified as V
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (KnownNat, Nat)
import Linear (V2 (V2))
import Prelude hiding (zipWith)

type Coord n = Coord' n n

type Coord' n m = (Finite n, Finite m)

fromPair :: (KnownNat n, KnownNat m) => (Integer, Integer) -> Maybe (Coord' n m)
fromPair = bitraverse packFinite packFinite

cardinal, cardinal', diagonals, diagonals', neighbours :: (KnownNat n) => Coord n -> [Coord n]
cardinal c = catMaybes [above c, below c, left c, right c]
cardinal' c = [above' c, below' c, left' c, right' c]
diagonals c = catMaybes [above =<< left c, above =<< right c, below =<< left c, below =<< right c]
diagonals' c = [above' $ left' c, above' $ right' c, below' $ left' c, below' $ right' c]
neighbours c = cardinal c ++ diagonals c

origin :: (KnownNat n, KnownNat m) => Coord' n m
origin = (0, 0)

north, south, east, west :: V2 Integer
north = V2 (-1) 0
south = V2 1 0
east = V2 0 1
west = V2 0 (-1)

turnLeft, turnRight, turnAround :: V2 Integer -> V2 Integer
turnLeft (V2 y x) = V2 (-x) y
turnRight (V2 y x) = V2 x (-y)
turnAround (V2 y x) = V2 (-y) (-x)

above, below, left, right :: (KnownNat n, KnownNat m) => Coord' n m -> Maybe (Coord' n m)
above = (`plus'` north)
left = (`plus'` west)
below = (`plus'` south)
right = (`plus'` east)

-- | These wrap around -- TODO Num instance for Coord n
above', below', left', right' :: (KnownNat n, KnownNat m) => Coord' n m -> Coord' n m
above' (y, x) = (y - 1, x)
below' (y, x) = (y + 1, x)
left' (y, x) = (y, x - 1)
right' (y, x) = (y, x + 1)

plus :: (KnownNat n, KnownNat m) => Coord' n m -> Coord' n m -> Maybe (Coord' n m)
plus = zipWith (+)

plus' :: (KnownNat n, KnownNat m) => Coord' n m -> V2 Integer -> Maybe (Coord' n m)
plus' = zipWith' (+)

zipWith :: (KnownNat n, KnownNat m) => (Integer -> Integer -> Integer) -> Coord' n m -> Coord' n m -> Maybe (Coord' n m)
zipWith f c (getFinite -> y2, getFinite -> x2) = zipWith' f c (V2 y2 x2)

zipWith' :: (KnownNat n, KnownNat m) => (Integer -> Integer -> Integer) -> Coord' n m -> V2 Integer -> Maybe (Coord' n m)
zipWith' f (getFinite -> y1, getFinite -> x1) (V2 y2 x2) = fromPair (f y1 y2, f x1 x2)

colines :: (KnownNat n, KnownNat m) => [Coord' n m] -> [Coord' n m]
colines coords = concat [colinear x y | x <- coords, y <- coords, x /= y]

colinear :: (KnownNat n, KnownNat m) => Coord' n m -> Coord' n m -> [Coord' n m]
colinear (getFinite -> y1, getFinite -> x1) (getFinite -> y2, getFinite -> x2) =
  unfoldr nextPoint 1
  where
    nextPoint t = (,t + 1) <$> fromPair (y1 + t * dy, x1 + t * dx)
    dy = y2 - y1
    dx = x2 - x1

-- | Square Grid
type Grid n a = Grid' n n a

type GridF n = GridF' n n

-- | 2D Grid
type GridF' n m = Compose (V.Vector n) (V.Vector m)

newtype Grid' (n :: Nat) (m :: Nat) a = G {unGrid :: Store (GridF' n m) a}
  deriving (Functor, ComonadStore (Coord' n m)) via (Store (GridF' n m))
  deriving (Generic, Generic1)

instance Foldable (Grid' n m) where
  foldMap f (unwrap . unGrid -> g) = foldMap f g

-- instance Traversable (Grid' n m) where
--   traverse f (unwrap . unGrid -> g) = store <$> traverse f g

instance (KnownNat n, KnownNat m) => Comonad (Grid' n m) where
  extract (G g) = extract g
  duplicate (G g) = coerce (coerce <$> duplicate g)

fromArray :: (KnownNat n) => Array (Int, Int) a -> Grid n a
fromArray = (`fromArray'` origin)

fromArray' :: (KnownNat n) => Array (Int, Int) a -> Coord n -> Grid n a
fromArray' arr = G . store ((arr !) . (fromIntegral *** fromIntegral))

(!?) :: (KnownNat n, KnownNat m) => Grid' n m a -> V2 Integer -> Maybe a
(!?) g = fmap extract . move' g

move :: (KnownNat n, KnownNat m) => Grid' n m a -> Coord' n m -> Maybe (Grid' n m a)
move g (getFinite -> y, getFinite -> x) = move' g (V2 y x)

move' :: (KnownNat n, KnownNat m) => Grid' n m a -> V2 Integer -> Maybe (Grid' n m a)
move' g = fmap (`seek` g) . plus' (pos g)

-- TODO I think the better way is to use the [[Data.Functor.Rep.tabulated]] lens
update :: (KnownNat n, KnownNat m) => Coord' n m -> a -> Grid' n m a -> Grid' n m a
update c = modify ((c ==) . pos)

modify :: forall n m a. (KnownNat n, KnownNat m) => (Grid' n m a -> Bool) -> a -> Grid' n m a -> Grid' n m a
modify f a = extend (\g -> if f g then a else extract g)

preview :: (Show a) => Grid' n m a -> String
preview = render (const show)

render :: (Coord' n m -> a -> String) -> Grid' n m a -> String
render renderSpace (getCompose . unwrap . unGrid -> g) =
  foldMap (\(x, rs) -> foldMap (uncurry (renderSpace . (x,))) (V.indexed rs) ++ "\n") (V.indexed g)

unwrap :: Store f a -> f a
unwrap (StoreT (Identity x) _) = x

toArray :: [[a]] -> Array (Int, Int) a
toArray xss = xss `seq` array bnds [((i, j), xss !! i !! j) | i <- [0 .. rows - 1], j <- [0 .. cols - 1]]
  where
    rows = length xss
    cols = length (head xss)
    bnds = ((0, 0), (rows - 1, cols - 1))