{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Day10 where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Vector (Vector)

import Data.ByteString (ByteString)

import Data.List
import Data.Either
import Data.Maybe
import Debug.Trace
import Data.Ord (comparing)
import Control.Arrow ((&&&))
import System.IO.Unsafe (unsafePerformIO)
import Prelude

-- * Part One

-- | p1
-- What is the winning Elf's score?
p1 :: ByteString -> String
p1 (parsePoints -> points) =
  let ext = extents points
      !_ = foldl' (go ext) points [0 .. 4]
  in show (length points)
  where
    go ext points _ =
      let !str = drawAscii ext points
          !_ = unsafePerformIO (putStr $ str <> "\n")
      in step points

step :: [Point] -> [Point]
step = fmap (\((pX, pY), v@(vX, vY)) -> ((pX + vX, pY + vY), v))

extents :: [Point] -> Extent
extents points =
  let x = (minimum &&& maximum) (fmap (fst . fst) points)
      y = (minimum &&& maximum) (fmap (snd . fst) points)
  in (x, y)

drawAscii :: Extent -> [Point] -> String
drawAscii ((minX, maxX), (minY, maxY)) pointsList =
  let pointsMap = M.fromList pointsList
  in concat
       [ (if x == minX then "\n" else "") <> (maybe "." (const "#") p)
       | y <- [minY .. maxY]
       , x <- [minX .. maxX]
       , let p = M.lookup (x, y) pointsMap
       ]


-- * Part Two

-- | p2
p2 :: ByteString -> Int
p2 _ =
  let x = 0
  in x

-- * Util
type Grid = M.Map P P
type P = (Int, Int)
type Extent = (P, P) -- * ( (minX, maxX) , (minY, maxY) )
type Point =  (P, P)

parsePoints :: ByteString -> [Point]
parsePoints =
  rights . fmap (parseOnly parsePoint) . B8.lines
  where
    parsePoint = do
      string "position=<"
      skipSpace
      pX <- signed decimal
      char ','
      skipSpace
      pY <- signed decimal
      string "> velocity=<"
      skipSpace
      vX <- signed decimal
      char ','
      skipSpace
      vY <- signed decimal
      pure ((pX, pY), (vX, vY))
