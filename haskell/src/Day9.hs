{-# LANGUAGE StrictData #-}

module Day9 where

import qualified Data.ByteString.Char8 as B8
import qualified Data.List.PointedList.Circular as L
import qualified Data.IntMap.Strict as M
import Data.Attoparsec.ByteString.Char8 as P

import Data.ByteString (ByteString)

import Data.List (foldl')
import Data.Either (fromRight)

import Prelude

data PState =
  PS Int -- * Current player
     (M.IntMap Int) -- * Player scores
     (L.PointedList Int) -- * Marble ring
  deriving (Show)

data Params =
  P Int -- * Number of Players 
    Int -- * Last Marble
  deriving (Show)

-- * Part One

-- | p1
-- What is the winning Elf's score?
p1 :: ByteString -> Int
p1 (parseParams -> params) =
  let pstate = runGame params
  in highScore pstate

runGame :: Params -> PState
runGame (P numPlayers lastMarble) = foldl' go initState [1 .. lastMarble]
  where
    initState = PS 0 M.empty (L.singleton 0)
    incPlayer player = (player + 1) `rem` numPlayers
    go (PS player scores ring) x =
      if (x `rem` 23 /= 0)
        then PS (incPlayer player) scores (L.insert x (L.next ring))
        else let (Just ring', f) =
                   let r' = L.moveN (-7) ring
                   in (L.delete r', L._focus r')
                 scores' = M.insertWith (+) player (x + f) scores
             in PS (incPlayer player) scores' ring'

highScore :: PState -> Int
highScore (PS _ scores _ ) = maximum (M.elems scores)

-- * Part Two

-- | p2
p2 :: ByteString -> Int
p2 (parseParams -> P np lm) =
  let pstate = runGame (P np (lm * 100))
  in highScore pstate

-- * Util

parseParams :: ByteString -> Params
parseParams =
  fromRight (error "parseParams") . parseOnly parser . head . B8.lines
  where
    parser =
      P <$> decimal <*> (string " players; last marble is worth " *> decimal)
