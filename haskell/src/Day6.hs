{-# LANGUAGE StrictData #-}

module Day6 where

import qualified Data.ByteString.Char8 as B8

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Vector (Vector)

import Data.List
import Data.Ord (comparing)
import Control.Arrow ((&&&))

import Prelude

type CoordId = Int -- the element index of Coords
type Coord = (Int, Int)
type Loc = (Int, Int)
type Board = M.Map Loc Int
data Params = Params
  { minX :: Int , maxX :: Int
  , minY :: Int , maxY :: Int
  , coords :: Vector Coord
  } deriving (Show)

-- * Part One

-- | p1
-- .. maybe you can minimize the danger by finding the coordinate that gives the largest distance from the other points
-- What is the size of the largest area that isn't infinite?
p1 :: ByteString -> Int
p1 (readInput -> input) =
  let params = toParams input
      board = toBoard params (closest (coords params)) 
      (_, area) = findLargestAreaByCoordId params board
  in area

findLargestAreaByCoordId :: Params -> Board -> (CoordId, Int)
findLargestAreaByCoordId p@(Params {..}) b =
  maximumBy (comparing snd) . M.assocs . toFreq . filter (not . isInf) $
  M.elems b
  where
    isInf c_ix = S.member c_ix inf_ixs
    inf_ixs = foldl' go S.empty (toBorder p)
      where
        go s l = maybe s (\c_ix -> S.insert c_ix s) (M.lookup l b)

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

toBoard :: Params -> (Loc -> Maybe Int)  -> Board
toBoard Params {..} f =
  let locs = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
  in foldl' go M.empty locs
  where
    go b loc = maybe b (\c_ix -> M.insert loc c_ix b) (f loc)

closest :: Vector Coord -> Loc -> Maybe CoordId
closest coords loc = 
  fst (V.ifoldl' go (Nothing, maxBound) coords)
  where
    go (mix', d') ix c =
      case dist loc c of
        d | d < d' -> (Just ix, d)
        d | d > d' -> (mix', d')
        d | d == d' -> (Nothing, d) -- discard ties
        _ -> error "N/A"

-- * Part Two

-- | p2
-- What is the size of the region containing all locations which have
-- a total distance to all given coordinates of less than 10000?
p2 :: ByteString -> Int
p2 = p2' 10000 

p2' :: Int -> ByteString -> Int
p2' distLimit (readInput -> input) =
  let params = toParams input
      board = toBoard params (\loc ->
                if isLocInRegion loc (coords params) 
                   then Just 1
                   else Nothing)
  in M.size board
  where
    isLocInRegion loc = (distLimit >) . sum . fmap (dist loc)

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
