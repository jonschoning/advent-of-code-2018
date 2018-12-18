{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Day14 where

import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Control.Applicative (liftA2)
import Data.List
import Control.Monad
import Data.Foldable
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- Lazy solution is slower than strict one, but is generative
import Control.Monad.ST.Lazy

import Prelude

-- * Part One

data Choc s =
  Choc Int -- ^ elf1 pos
       Int -- ^ elf2 pos
       Int -- ^ scores length
       (MV.STVector s Int) -- ^ scores

-- | p1
p1 :: ByteString -> String
p1 (B8.readInt -> Just (initialSize, _)) =
  runST $ do
    let finalScoreSize = 10
    recipies <- createRecipies 1000
    let r = concatMap show (take finalScoreSize (drop initialSize recipies))
    pure r

createRecipies :: Int -> ST s [Int]
createRecipies end_size = do
    scores <- strictToLazyST $ do
      scores <- MV.unsafeNew (end_size + 1)
      MV.unsafeWrite scores 0 3
      MV.unsafeWrite scores 1 7
      pure scores
    ([3,7] ++) <$> (step (Choc 0 1 2 scores))
  where
    step :: (Choc s) -> ST s [Int]
    step (Choc e1 e2 size scores) = do
      scores' <- strictToLazyST $ if (size + 2 >= MV.length scores) then (MV.unsafeGrow scores 1000000) else pure scores
      (rs1, rs2) <- strictToLazyST $ liftA2 (,) (MV.unsafeRead scores' e1) (MV.unsafeRead scores' e2)
      let rss = rs1 + rs2
      news <-
        if rss >= 10
          then do
            strictToLazyST $ do
              MV.unsafeWrite scores' size 1
              MV.unsafeWrite scores' (size + 1) (rss - 10)
              pure [1, rss - 10]
          else do
            strictToLazyST $ do
             MV.unsafeWrite scores' size rss
             pure [rss]
      let size'= size + length news
      let e1' = (e1 + rs1 + 1) `mod` size'
          e2' = (e2 + rs2 + 1) `mod` size'
      (news ++) <$> (step (Choc e1' e2' size' scores'))

-- | p2
p2 :: ByteString -> Int
p2 (B8.readInt -> Just (initialSize, _)) =
  runST $ do
    recipies <- createRecipies 1000
    let r_search = (fmap (read . pure) (show initialSize)) :: [Int]
        r_len = length r_search
        find_r_search r = take r_len r == r_search
    pure $ fromJust (findIndex find_r_search (tails recipies))

