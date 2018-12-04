{-# LANGUAGE StrictData #-}

module Day3 where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe
import Prelude

type ClaimId = B8.ByteString

data Claim =
  Claim ClaimId -- * id
        Int -- * left
        Int -- * top
        Int -- * width
        Int -- * height
  deriving (Show)

data Coord =
  Coord Int
        Int
  deriving (Ord, Eq, Show)

-- * Part One

-- | p1
-- How many square inches of fabric are within two or more claims?
p1 :: B8.ByteString -> Int
p1 = length . filter (> 1) . M.elems . overlapCount . parseInput

overlapCount :: [Claim] -> M.Map Coord Int
overlapCount = foldl' go M.empty
  where
    go m c = foldl' (\m' k -> M.insertWith (+) k 1 m') m (toCoords c)

toCoords :: Claim -> [Coord]
toCoords (Claim _ l t w h) =
  let xs = take w (iterate succ l)
      ys = take h (iterate succ t)
  in [Coord x y | x <- xs, y <- ys]

-- * Part Two

-- | p2
-- What is the ID of the only claim that doesn't overlap?
p2 :: B8.ByteString -> B8.ByteString
p2 (parseInput -> claims) =
  fromJust $ findNonOverlappingClaim (overlapId claims) claims 

overlapId :: [Claim] -> M.Map Coord ClaimId
overlapId = foldl' go M.empty
  where
    go m c@(Claim _id _ _ _ _) =
      foldl' (\m' k -> M.insertWith (\_ _ -> "X") k _id m') m (toCoords c)

findNonOverlappingClaim :: M.Map Coord ClaimId -> [Claim] -> Maybe ClaimId
findNonOverlappingClaim m = listToMaybe . mapMaybe go
  where
    go c@(Claim _id _ _ _ _) =
      if all (== Just _id) $ fmap (\k -> M.lookup k m) (toCoords c)
        then Just _id
        else Nothing

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
