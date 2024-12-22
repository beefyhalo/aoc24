{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day9 (module Day9) where

import Control.Applicative (optional)
import Control.Comonad.Cofree (Cofree, unfoldM)
import Control.Comonad.Cofree qualified as Cofree
import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Data.Array ()
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 (digit)
import Data.Char (digitToInt)
import Data.Coerce (coerce)
import Data.Functor.Foldable (cata)
import Data.Heap qualified as H
import Data.Maybe (isNothing)
import Data.Ord (Down (Down))
import Data.Sequence qualified as Seq
import Debug.Trace
import GHC.IO (unsafePerformIO)

data OddListF a b r = OneF a | ConsF a b r
  deriving (Show, Functor, Foldable, Traversable)

newtype FileBlock = FileBlock Int
  deriving (Show, Eq)
  deriving (Ord) via (Down Int)

newtype FreeSpace = FreeSpace Int
  deriving (Show)

type Sector = Cofree (OddListF FileBlock FreeSpace)

newtype FileId = FileId Int
  deriving (Show, Eq)
  deriving (Ord) via (Down Int)

type Input = Sector FileId

checksum :: [FileId] -> Int
checksum = sum . zipWith (*) [0 ..] . coerce

expand :: Sector a -> Seq.Seq (Maybe a)
expand = cata $ \case
  l :< OneF (FileBlock fb) -> Seq.replicate fb (Just l)
  l :< ConsF (FileBlock fb) (FreeSpace fs) rest -> mconcat [Seq.replicate fb (Just l), Seq.replicate fs Nothing, rest]

solution :: Input -> Int
solution = checksum . smash . expand
  where
    smash :: Seq.Seq (Maybe a) -> [a]
    smash s = case (Seq.viewl s, Seq.viewr s) of
      (Just x Seq.:< rest, _) -> x : smash (ff rest) -- Add x from the front
      (_, rest Seq.:> Just y) -> y : smash (Seq.drop 1 rest) -- Add y from the back
      (Nothing Seq.:< _, rest Seq.:> Nothing) -> smash (ff rest)
      _ -> []
      where
        ff = Seq.dropWhileR isNothing -- Fast forward to the next Just

partTwo :: Input -> Int
partTwo i = solution i
  where
    w = collectBlocks i
    x = insertBlocks w (coerce <$> i)
    !_ = unsafePerformIO $ print w
    !_ = unsafePerformIO $ print $ expand x

    collectBlocks :: (Ord a) => Sector a -> H.Heap (H.Entry FileBlock (H.Heap a))
    collectBlocks = cata go
      where
        go (l :< OneF block) = H.singleton (H.Entry block (H.singleton l))
        go (l :< ConsF block _ rest) = H.insert (H.Entry block (H.singleton l)) rest

    -- collectBlocks :: (Ord a) => Sector a -> H.Heap (H.Entry FileBlock (H.Heap a))
    -- collectBlocks = cata go
    --   where
    --     go (l :< OneF block) = H.singleton (H.Entry block (H.singleton l))
    --     go (l :< ConsF block _ rest) =
    --       H.map (\(H.Entry b h) -> if b == block then H.Entry b (H.insert l h) else H.Entry b h) rest

    insertBlocks :: (Ord a, Show a) => H.Heap (H.Entry FileBlock (H.Heap a)) -> Sector a -> Sector a
    insertBlocks _ one@(_ Cofree.:< OneF _) = one
    insertBlocks blocks (label Cofree.:< ConsF block (FreeSpace space) rest) = traceShow ("b", blocksThatFit) $ case H.viewMin blocksThatFit of
      Just (H.Entry foundBlock labels, newHeap) -> insertBlock (H.minimum labels) foundBlock newHeap
      _ -> continue
      where
        blocksThatFit = snd $ H.span (<= H.Entry (FileBlock space) undefined) blocks
        continue = traceShow ("continue", label, block, space) $ label Cofree.:< ConsF block (FreeSpace space) (insertBlocks blocks rest)
        insertBlock label2 newBlock@(FileBlock blockSize) newHeap =
          traceShow ("insert", label, block, "and then", label2, blockSize) $
            let next = label2 Cofree.:< ConsF newBlock (FreeSpace $ space - blockSize) rest
                updatedHeap = undefined newHeap
             in label Cofree.:< ConsF block (FreeSpace 0) (insertBlocks updatedHeap next)

-- collectBlocks :: (Ord a) => Sector a -> H.Heap (H.Entry FileBlock (H.Heap (H.Entry a a)))
-- collectBlocks = cata go
--   where
--     go (l :< OneF block) = H.singleton (H.Entry block (H.singleton $ H.Entry l l))
--     go (l :< ConsF block _ rest) =
--             H.map (\(H.Entry b h) -> if b == block then H.Entry b (H.insert (H.Entry l l) h) else H.Entry b h) rest

-- insertBlocks :: (Ord a, Show a) => H.Heap (H.Entry FileBlock (H.Heap (H.Entry a a))) -> Sector a -> Sector a
-- insertBlocks _ one@(_ Cofree.:< OneF _) = one
-- insertBlocks blocks (label Cofree.:< ConsF block (FreeSpace space) rest) = traceShow ("b", blocksThatFit) $ case H.viewMin blocksThatFit of
--   Just (H.Entry foundBlock labels, newHeap) -> undefined -- insertBlock (H.minimum labels) foundBlock newHeap
--   _ -> continue
--   where
--     blocksThatFit = snd $ H.span (<= H.Entry (FileBlock space) undefined) blocks
--     continue = traceShow ("continue", label, block, space) $ label Cofree.:< ConsF block (FreeSpace space) (insertBlocks blocks rest)
--     insertBlock label2 newBlock@(FileBlock blockSize) newHeap =
--       traceShow ("insert", label, block, "and then", label2, blockSize) $
--         let next = label2 Cofree.:< ConsF newBlock (FreeSpace $ space - blockSize) rest
--             updatedHeap = undefined newHeap
--          in label Cofree.:< ConsF block (FreeSpace 0) (insertBlocks updatedHeap next)

parser :: Parser Input
parser = unfoldM (flip fmap sector . next) 0
  where
    next i (b, s) = (FileId i, maybe (OneF b) (flip (ConsF b) (i + 1)) s)
    intDigit = digitToInt <$> digit

    sector :: Parser (FileBlock, Maybe FreeSpace)
    sector = (\c c2 -> (FileBlock c, FreeSpace <$> c2)) <$> intDigit <*> optional intDigit

-- insertBlocks m sector@(l Cofree.:< ConsF block (FreeSpace space) rest) = case Map.lookupLT (FileBlock space) m of
--   Just (FileBlock fb2, heap) -> case H.viewMin heap of
--     Just (l2, newHeap)
--       | l == l2 -> insertBlocks (newMap heap) sector
--       | otherwise -> insertBlock l2 newHeap
--     _ -> continue
--     where
--       newMap newHeap = Map.insert (FileBlock fb2) newHeap m
--       insertBlock l2 newHeap =
--         l
--           Cofree.:< ConsF
--             block
--             (FreeSpace 0)
--             ( insertBlocks (newMap newHeap) $
--                 l2 Cofree.:< ConsF (FileBlock fb2) (FreeSpace (space - fb2)) rest
--             )
--   _ -> continue
--   where
--     continue = l Cofree.:< ConsF block (FreeSpace space) (insertBlocks m rest)

-- where
--   go _
--   go m x@(l Cofree.:< ConsF block (FreeSpace space) rest) = case Map.lookupGT (FileBlock $ space + 1 ) m of
--     Just (FileBlock fb2, heap) -> case H.viewMin heap of
--       Just (l2, newHeap)
--         | l == l2 -> traceShow ("0000000", newHeap) $ go (Map.insert (FileBlock fb2) (newHeap) m) x
--         | otherwise ->
--             traceShow ("AA", l, block, l2 ) $
--               l Cofree.:< ConsF block (FreeSpace 0) (go (Map.insert (FileBlock fb2) newHeap m) $ l2 Cofree.:< ConsF (FileBlock fb2) (FreeSpace (space - fb2)) rest)
--       -- \| otherwise -> traceShow ("ASDASDASDASDASDASDASDASDADASD", l, l2, l > l2) undefined
--       _ -> traceShow ("BBBB", l, space) $ l Cofree.:< ConsF block (FreeSpace space) (go m rest)
--     _ -> traceShow ("CCCCCCC", l, block, space) $ l Cofree.:< ConsF block (FreeSpace space) (go m rest)

-- insertBlocks :: forall a. (Show a) => [(a, FileBlock)] -> Sector a -> Sector a
-- insertBlocks = flip (foldl insert)
--   where
--     insert :: Sector a -> (a, FileBlock) -> Sector a
--     insert currentSector (label, block) =
--       fromMaybe currentSector $ insertBlock label block currentSector

-- insertBlocks :: forall a. (Show a) => [(a, FileBlock)] -> Sector a -> Sector a
-- insertBlocks blocks = go . duplicate
--   where
--     go :: Sector (Sector a) -> Sector a
--     go (l Cofree.:< OneF _) = l
--     go (l Cofree.:< ConsF block free rest) = foldl insert _ _

--     insert :: Sector a -> (a, FileBlock) -> Sector a
--     insert currentSector (label, block) = insertBlock label block currentSector

-- adjustFreeSpace :: (Show a, Ord a) => a -> Sector a -> Sector a
-- adjustFreeSpace l = cata go
--   where
--     go (l2 :< xs) | l == l2 = traceShow ("FFFFFFFFFFFFFFFFFFFFF") $ case xs of
--       OneF _ -> l Cofree.:< OneF (FileBlock 0)
--       ConsF fb (FreeSpace s) rest2 -> l Cofree.:< ConsF (FileBlock 0) (FreeSpace s) rest2
--     go (l :< x) = l Cofree.:< x

-- go (l2 :< ConsF fb (FreeSpace s) ) = Map.insert (extract sector) space rest

-- freeSpaceMap :: forall a. (Ord a) => Sector a -> Map.Map a FreeSpace
-- freeSpaceMap = cata go . duplicate
--   where
--     go (_ :< OneF _) = Map.empty
--     go (sector :< ConsF _ space rest) = Map.insert (extract sector) space rest

-- insertBlocks :: forall a. (Ord a, Show a) => [(a, FileBlock)] -> Sector a -> Sector a
-- insertBlocks = flip (foldl insert)
--   where
--     insert :: Sector a -> (a, FileBlock) -> Sector a
--     insert currentSector (label, block) =
--       fromMaybe currentSector $ insertBlock label block currentSector

-- insertBlock :: (Ord a, Show a) => a -> FileBlock -> Sector a -> Maybe (Sector a)
-- insertBlock label _ (_ Cofree.:< OneF _) = traceShow ("Z", label) Nothing
-- insertBlock label block@(FileBlock size) (l Cofree.:< ConsF fb (FreeSpace space) rest)
--   | size <= space =
--       let updatedFreeSpace = FreeSpace (space - size)
--        in traceShow ("A", label, l, toList rest) $
--             Just $
--               l Cofree.:< ConsF fb (FreeSpace 0) (label Cofree.:< ConsF block updatedFreeSpace (adjustFreeSpace label rest))
--   | otherwise = (\x -> l Cofree.:< ConsF fb (FreeSpace space) x) <$> insertBlock label block rest
