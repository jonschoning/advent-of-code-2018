{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Day3 where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import Data.List
import Data.Monoid
import Data.Maybe
import Prelude
import Debug.Trace

data Claim =
  Claim B8.ByteString -- * id
        Int -- * left
        Int -- * top
        Int -- * width
        Int -- * height
  deriving (Show)

data Pr =
  Pr Int
     Int
  deriving (Ord, Eq)

-- * Part One

-- | p1
-- How many square inches of fabric are within two or more claims?
p1 :: B8.ByteString -> Int
p1 (parseInput -> claims) =
  M.foldl' (\acc v -> if v > 1 then succ acc else acc) 0 (overlapCount claims)

overlapCount :: [Claim] -> M.Map Pr Int
overlapCount = foldl' go M.empty
  where
    go m claim = foldl' (\m' k -> M.insertWith (+) k 1 m') m (toCoords claim)

toCoords :: Claim -> [Pr]
toCoords (Claim _ l t w h) =
  let xs = take w (iterate succ l)
      ys = take h (iterate succ t)
  in [Pr x y | x <- xs, y <- ys]

-- * Part Two

-- | p2
-- What is the ID of the only claim that doesn't overlap?
p2 :: B8.ByteString -> B8.ByteString
p2 (parseInput -> claims) = "0"
  
-- * Util

parseInput :: B8.ByteString -> [Claim]
parseInput input = do
  [_i, _, _lt, _wh] <- B8.words <$> B8.lines input
  let i = B8.drop 1 _i
      Just (l, _lt') = B8.readInt _lt
      Just (t, _) = B8.readInt (B8.drop 1 _lt')
      Just (w, _wh') = B8.readInt _wh
      Just (h, _) = B8.readInt (B8.drop 1 _wh')
  pure $ Claim i l t w h
