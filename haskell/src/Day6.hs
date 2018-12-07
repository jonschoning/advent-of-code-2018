{-# LANGUAGE StrictData #-}

module Day6 where

import qualified Data.ByteString.Char8 as B8

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Vector (Vector)

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Ord
import Control.Arrow

import Prelude

type Coords = Vector Coord
type Coord = (Int, Int)
type Loc = (Int, Int)
type Board = M.Map Loc Int
data Params = Params
  { minX :: Int , maxX :: Int
  , minY :: Int , maxY :: Int
  , coords :: Coords
  } deriving (Show)

-- * Part One

-- | p1
p1 :: ByteString -> Int
p1 (readInput -> input) =
  let params = toParams input
      board = toBoard params
      (_, freq) = findLargest params board
  in freq

findLargest :: Params -> Board -> (Int, Int)
findLargest p@(Params {..}) b =
  (maximumBy cmp . M.toList . toFreq . M.elems) b
  where
    cmp (c_ix, freq) (c_ix', freq') =
      case (isInf c_ix, isInf c_ix') of
        (False, False) -> compare freq freq'
        (False, True) -> GT
        (True, False) -> LT
        (True, True) -> EQ
    isInf c_ix = S.member c_ix inf_ixs
    inf_ixs = foldl' go S.empty (toBorder p)
      where go s l = maybe s (\c_ix -> S.insert c_ix s) (M.lookup l b)

toBorder :: Params -> S.Set Coord
toBorder (Params {..}) = 
  let (xs, xs') = (fmap (, minY) &&& fmap (, maxY)) [minX .. maxX]
      (ys, ys') = (fmap (minX, ) &&& fmap (maxX, )) [minY .. maxY]
  in S.fromList $ xs ++ xs' ++ ys ++ ys'

toParams :: [Coord] -> Params
toParams cs =
  let (minX, maxX) = (minimum &&& maximum) (fmap fst coords)
      (minY, maxY) = (minimum &&& maximum) (fmap snd coords)
      coords = V.fromList cs
  in Params {..}

toBoard :: Params -> Board
toBoard Params {..} =
  let locs = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
  in foldl' go M.empty locs
  where
    go b loc =
      case closest coords loc of
        Just c_ix -> M.insert loc c_ix b
        Nothing -> b

closest :: Coords -> Loc -> Maybe Int
closest coords loc = 
  fst (V.ifoldl' go (Nothing, maxBound) coords)
  where
    go (mix', d') ix c =
      case dist loc c of
        d | d < d' -> (Just ix, d)
        d | d > d' -> (mix', d')
        d | d == d' -> (Nothing, d)
        _ -> error "N/A"

-- * Part Two

-- | p2
-- What is the size of the region containing all locations which have
-- a total distance to all given coordinates of less than 10000?
p2 :: ByteString -> Int
p2 input =
  let x = 0
  in 0

-- * Util

toFreq :: Ord a => [a] -> M.Map a Int
toFreq = M.fromListWith (+) . fmap (, 1 :: Int)
{-# INLINE toFreq #-}

dist :: Loc -> Loc -> Int
dist (x1, y1) (x2, y2) = dist' x1 y1 x2 y2
{-# INLINE dist #-}

dist' :: Int -> Int -> Int -> Int -> Int
dist' x1 y1 x2 y2 = abs (x2 - x1) + abs (y2 - y1)
{-# INLINE dist' #-}

readInput :: ByteString -> [Coord]
readInput = fmap readCoord . B8.lines
  where
    readCoord xs =
      let Just (x, xs') = B8.readInt xs
          Just (y, _) = B8.readInt (B8.drop 2 xs')
      in (x, y)
