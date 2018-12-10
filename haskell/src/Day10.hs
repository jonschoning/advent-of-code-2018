{-# LANGUAGE StrictData #-}

module Day10 where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Set as S
import Data.Attoparsec.ByteString.Char8 as P

import Data.ByteString (ByteString)

import Data.List (unfoldr)
import Data.Either (rights)
import Control.Arrow ((&&&))
import System.IO.Unsafe (unsafePerformIO)
import Data.Function ((&))

import Prelude

-- * Part One

type Point =  (Position, Velocity)
type Position = (Int, Int) -- * (pX, pY)
type Velocity = (Int, Int) -- * (vX, vY)

-- | p1
-- What message will eventually appear in the sky?
p1 :: ByteString -> ByteString
p1 (parsePoints -> points) = last (render points)

render :: [Point] -> [ByteString]
render points' = unfoldr go (maxBound, points')
  where
    go (xRange, points) =
      let b@((minX, maxX), _) = bounds points
          xRange' = maxX - minX
      in if xRange' > xRange
           then Nothing
           else Just (render' b (fmap fst points), (xRange', step points))
    bounds points =
      let xx = (minimum &&& maximum) (fmap (fst . fst) points)
          yy = (minimum &&& maximum) (fmap (snd . fst) points)
      in (xx, yy)
    step = fmap (\((pX, pY), v@(vX, vY)) -> ((pX + vX, pY + vY), v))
    render' ((minX, maxX), (minY, maxY)) (S.fromList -> pointSet) =
      [ [ if S.member (x, y) pointSet then '#' else '.'
        | x <- [minX .. maxX] ]
      | y <- [minY .. maxY] ]
      & unlines
      & B8.pack

-- * Part Two

-- | p2
-- exactly how many seconds would they have needed to wait for that message to appear?
p2 :: ByteString -> Int
p2 (parsePoints -> points) = length (render points) - 1

-- * Util

parsePoints :: ByteString -> [Point]
parsePoints =
  rights . fmap (parseOnly parsePoint) . B8.lines
  where
    parsePoint = do
      _ <- string "position=<"
      pX <- skipSpace *> signed decimal
      pY <- char ',' *> skipSpace *> signed decimal
      _ <- string "> velocity=<" 
      vX <- skipSpace *> signed decimal
      vY <- char ',' *> skipSpace *> signed decimal
      pure ((pX, pY), (vX, vY))

printIO :: ByteString -> ()
printIO =
  (\(!_) -> ()) .
  unsafePerformIO .
  B8.putStr

-- printListIO :: [ByteString] -> ()
-- printListIO =
--   (\(!_) -> ()) .
--   unsafePerformIO .
--   mapM (\(i, x) -> B8.putStr $ ((B8.pack . show) i) <> x <> "\n") .
--   zip [0 :: Int ..]
