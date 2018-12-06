
module Day5 where

import qualified Data.ByteString.Char8 as B8

import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Foldable

import Prelude

-- * Part One

-- | p1
-- How many units remain after fully reacting the polymer you scanned?
p1 :: ByteString -> Int
p1 (readInput -> polymer) =
  length (react polymer)

react :: String -> String
react = reverse . foldl' go []
  where
    go [] c = [c]
    go xxs@(x:xs) c =
      if x /= c && toLower x == toLower c
        then xs
        else c : xxs

-- * Part Two

-- | p2
-- What is the length of the shortest polymer you can produce by
-- removing all units of exactly one type and fully reacting the result?
p2 :: ByteString -> Int
p2 (readInput -> polymer) =
  minimum $
  fmap length
    [ react polymer'
    | x <- ['a' .. 'z']
    , let polymer' = filter ((x /= ) . toLower) polymer
    ]


-- * Util

readInput :: ByteString -> String
readInput = B8.unpack . head . B8.lines
