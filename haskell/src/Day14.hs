{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Day14 where

import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Control.Applicative (liftA2)
import Data.List (findIndex)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Control.Monad.ST (runST, ST)

import Prelude

-- * Part One
  
data Choc s =
  Choc Int -- ^ elf1 pos
       Int -- ^ elf2 pos
       Int -- ^ scores length
       (MV.STVector s Int) -- ^ scores
  
getScores :: Choc s -> MV.STVector s Int
getScores (Choc _ _ _ scores) = scores

getSize :: Choc s -> Int
getSize (Choc _ _ size _) = size

-- | p1
p1 :: ByteString -> String
p1 (B8.readInt -> Just (initialSize, _)) =
  let finalScoreSize = 10
      recipies = createRecipies (initialSize + finalScoreSize)
  in concatMap show (V.toList (V.slice initialSize finalScoreSize recipies))


createRecipies :: Int -> V.Vector Int
createRecipies end_size =
  runST $ do
    scores <- MV.unsafeNew (end_size + 1)
    MV.unsafeWrite scores 0 3
    MV.unsafeWrite scores 1 7
    choc' <- step (Choc 0 1 2 scores)
    V.unsafeFreeze (getScores choc')
  where
    step :: Choc s -> ST s (Choc s)
    step choc@(getSize -> size)
      | size >= end_size = pure choc
    step (Choc e1 e2 size scores) = do
      (rs1, rs2) <- liftA2 (,) (MV.unsafeRead scores e1) (MV.unsafeRead scores e2)
      let rss = rs1 + rs2
      size' <-
        if rss >= 10
          then do
            MV.unsafeWrite scores size 1
            MV.unsafeWrite scores (size + 1) (rss - 10)
            pure (size + 2)
          else do
            MV.unsafeWrite scores size rss
            pure (size + 1)
      let e1' = (e1 + rs1 + 1) `mod` size'
          e2' = (e2 + rs2 + 1) `mod` size'
      step (Choc e1' e2' size' scores)

-- | p2
p2 :: ByteString -> Int
p2 (B8.readInt -> Just (initialSize, _)) =
  let finalScoreSize = 10
      recipies = createRecipies (initialSize + finalScoreSize) -- + 100000000)
      r_search = V.fromList (fmap (read . pure) (show initialSize))
      r_len = V.length r_search
      find_r_search r = V.unsafeSlice 0 r_len r == r_search
  in fromJust (findIndex find_r_search (vtails recipies))
  where
    vtails v
      | V.length v == 0 = []
    vtails v = v : vtails (V.tail v)

