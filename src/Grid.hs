{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grid (module Grid) where

import Control.Comonad (Comonad, duplicate, extract)
import Control.Comonad.Identity (Identity (Identity))
import Control.Comonad.Representable.Store (ComonadStore, Store, StoreT (StoreT), pos, runStore, seek, store)
import Data.Array.IArray (Array, array, (!))
import Data.Coerce (coerce)
import Data.Distributive (Distributive (distribute))
import Data.Finite (Finite, getFinite, packFinite)
import Data.Foldable (fold)
import Data.Foldable.WithIndex (FoldableWithIndex (ifoldMap))
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.List (unfoldr)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Vector.Sized qualified as V
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (KnownNat, Nat)
import Linear (V2 (V2))
import Prelude hiding (zipWith)

type Coord n = Coord' n n

newtype Coord' n m = C (Finite n, Finite m)
  deriving newtype (Ord, Eq, Show)

-- | Vector arithmetic
instance (KnownNat n, KnownNat m) => Num (Coord' n m) where
  (+) = zipCoord (+)
  {-# INLINE (+) #-}
  (-) = zipCoord (-)
  {-# INLINE (-) #-}
  (*) = zipCoord (*)
  {-# INLINE (*) #-}
  negate = error "nope"
  {-# INLINE negate #-}
  abs = id
  {-# INLINE abs #-}
  signum = id
  {-# INLINE signum #-}
  fromInteger = (\i -> C (i, coerce i)) . fromInteger
  {-# INLINE fromInteger #-}

cardinal, diagonals, neighbours :: (KnownNat n) => Coord n -> [Coord n]
cardinal c = catMaybes [above c, below c, left c, right c]
diagonals c = catMaybes [above =<< left c, above =<< right c, below =<< left c, below =<< right c]
neighbours = mconcat [cardinal, diagonals]

origin :: (KnownNat m, KnownNat n) => Coord' n m
origin = C (0, 0)

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
above = (`plus` north)
left = (`plus` west)
below = (`plus` south)
right = (`plus` east)

plus :: (KnownNat n, KnownNat m) => Coord' n m -> V2 Integer -> Maybe (Coord' n m)
plus (C (getFinite -> y1, getFinite -> x1)) (V2 y2 x2) = liftA2 (curry C) (packFinite $ y1 + y2) (packFinite $ x1 + x2)

zipCoord :: (KnownNat n, KnownNat m) => (Integer -> Integer -> Integer) -> Coord' n m -> Coord' n m -> Coord' n m
zipCoord f (C (getFinite -> y1, getFinite -> x1)) (C (getFinite -> y2, getFinite -> x2)) = C (fromIntegral $ f y1 y2, fromIntegral $ f x1 x2)

colines :: (KnownNat n, KnownNat m) => [Coord' n m] -> [Coord' n m]
colines coords = concat [colinear x y | x <- coords, y <- coords, x /= y]

colinear :: (KnownNat n, KnownNat m) => Coord' n m -> Coord' n m -> [Coord' n m]
colinear i@(C (getFinite -> y1, getFinite -> x1)) (C (getFinite -> y2, getFinite -> x2)) =
  unfoldr nextPoint 1
  where
    nextPoint t = (,t + 1) <$> i `plus` V2 (t * dy) (t * dx)
    dy = y2 - y1
    dx = x2 - x1

-- | Square Grid
type Grid n a = Grid' n n a

type GridF n = GridF' n n

-- | 2D Grid
newtype GridF' n m a = GridF' {unGridF :: Compose (V.Vector n) (V.Vector m) a}
  deriving newtype (Functor, Foldable)

instance FoldableWithIndex (Coord' n m) (GridF' n m) where
  ifoldMap = undefined

instance Distributive (GridF' n m) where
  distribute = distributeRep

instance Representable (GridF' n m) where
  type Rep (GridF' n m) = Coord' n m
  tabulate = undefined
  index = undefined

newtype Grid' (n :: Nat) (m :: Nat) a = G {unGrid :: Store (GridF' n m) a}
  deriving (Functor, ComonadStore (Coord' n m)) via (Store (GridF' n m))
  deriving (Generic, Generic1)

instance (Show a) => Show (Grid' n m a) where
  show = preview

instance Foldable (Grid' n m) where
  foldMap f (G (StoreT (Identity g) _)) = foldMap f g

instance FoldableWithIndex (Coord' n m) (Grid' n m) where
  ifoldMap f (G (StoreT (Identity g) _)) = ifoldMap f g

-- instance Traversable (Grid' n m) where
--   traverse f (unwrap . unGrid -> g) = store <$> traverse f g

instance (KnownNat n, KnownNat m) => Comonad (Grid' n m) where
  extract (G g) = extract g
  duplicate (G g) = coerce (coerce <$> duplicate g)

fromArray :: (KnownNat n, KnownNat m) => Array (Int, Int) a -> Grid' n m a
fromArray = (`fromArray'` origin)

fromArray' :: (KnownNat n, KnownNat m) => Array (Int, Int) a -> Coord' n m -> Grid' n m a
fromArray' arr = G . store (\(C (y, x)) -> arr ! (fromIntegral y, fromIntegral x))

fromMap :: (KnownNat n, KnownNat m, Monoid a) => Map.Map (Coord' n m) a -> Grid' n m a
fromMap m = G $ store (fold . (m Map.!?)) origin

(!?) :: (KnownNat n, KnownNat m) => Grid' n m a -> V2 Integer -> Maybe a
(!?) g = fmap extract . move g

-- seeks to the current position plus V2 Integer
move :: (KnownNat n, KnownNat m) => Grid' n m a -> V2 Integer -> Maybe (Grid' n m a)
move g = fmap (`seek` g) . plus (pos g)

-- TODO I think the better way is to use the [[Data.Functor.Rep.tabulated]] lens
update :: Coord' n m -> a -> Grid' n m a -> Grid' n m a
update c = modify (c ==)

modify :: (Coord' n m -> Bool) -> a -> Grid' n m a -> Grid' n m a
modify f a (runStore . unGrid -> (s, i)) = G $ store (\c -> if f c then a else s c) i -- extend (\g -> if f g then a else extract g)

preview :: (Show a) => Grid' n m a -> String
preview = render (const show)

render :: (Coord' n m -> a -> String) -> Grid' n m a -> String
render renderSpace (unwrap -> g) =
  foldMap (\(x, rs) -> foldMap (uncurry (renderSpace . C . (x,))) (V.indexed rs) ++ "\n") (V.indexed g)

unwrap :: Grid' n m a -> V.Vector n (V.Vector m a)
unwrap (G (StoreT (Identity (GridF' (Compose v))) _)) = v

toArray :: [[a]] -> Array (Int, Int) a
toArray xss = xss `seq` array bnds [((i, j), xss !! i !! j) | i <- [0 .. rows - 1], j <- [0 .. cols - 1]]
  where
    rows = length xss
    cols = length (head xss)
    bnds = ((0, 0), (rows - 1, cols - 1))