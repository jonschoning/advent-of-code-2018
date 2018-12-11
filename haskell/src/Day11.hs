{-# LANGUAGE StrictData #-}

-- TODO: replace solution with a dynamic programming algorithm that memoizes more values efficiently
-- e.g. https://en.wikipedia.org/wiki/Summed-area_table

module Day11 where

import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Control.Monad.State (State, execState, get, put)
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace (traceShowId)

import Prelude

-- * Part One
maxSize, maxY, minY, maxX, minX :: Int;
minX = 1; minY = 1;
maxX = 300; maxY = 300;
maxSize = 300;

type Serial = Int
type Size = Int
type TotalPower = Int
type Power = Int
type Coord = (Int, Int)
type Grid = M.Map Coord Power
type StateP = (MaxP, M.Map (Size, Coord) TotalPower)

data MaxP =
  MaxP Coord -- * (Int, Int)
       Size -- * Int
       TotalPower -- * Int
  
-- | p1
-- What is the X,Y coordinate of the top-left fuel cell of the 3x3
-- square with the largest total power?

p1 :: ByteString -> Coord
p1 (read . B8.unpack -> sn) =
  let size = 3
      g = grid sn
      (MaxP mc _ _, _) =
        sequence_
          [ totalPower g size (x, y)
          | x <- [minX .. maxX]
          , y <- [minY .. maxY]
          , x + size <= maxX && y + size <= maxY
          ]
        `execState` initialState
  in mc

initialState :: StateP
initialState =  (MaxP (0, 0) 0 0, M.empty)

-- | p2
-- What is the X,Y,size identifier of the square with the largest total power?
p2 :: ByteString -> (Int, Int, Int)
p2 (read . B8.unpack -> sn) =
  let g = grid sn
      (MaxP (mx, my) ms mz, _) =
        sequence_
          [ totalPower g size (x, y)
          | x <- [minX .. maxX]
          , y <- [minY .. maxY]
          , size <- [1 .. maxSize]
          , x + size <= maxX && y + size <= maxY
          ]
        `execState` initialState
  in (mx, my, ms)

-- * Util

totalPower :: Grid -> Size -> Coord -> State StateP Int
totalPower g 1 xy = pure (fromJust (M.lookup xy g))
totalPower g !size xy@(x, y) = do
  (oldMax@(MaxP _ _ mtp), m) <- get
  case M.lookup (size, xy) m of
    Just z -> pure z
    Nothing -> do
      tp0 <- (totalPower g (size - 1) xy)
      let tp = tp0 + (sum (mapMaybe ((flip M.lookup) g) $
                            ((, y + size - 1) <$> [x .. x + size - 1]) ++
                            ((x + size - 1, ) <$> [y .. y + size - 2])))
      put ( if tp > mtp then MaxP xy size tp else oldMax
          , M.insert (size, xy) tp m
          )
      pure tp

grid :: Serial -> Grid
grid sn =
  M.fromList
    [((x, y), powerAt sn (x, y)) | x <- [minX .. maxX], y <- [minY .. maxY]]

powerAt :: Serial -> Coord -> Int
powerAt sn (x, y) =
  let rackId = 10 + x
  in rackId * (sn + (y * rackId)) `div` 100 `mod` 10 - 5
