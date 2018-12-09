{-# LANGUAGE StrictData #-}

module Day8 where

import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec.ByteString.Char8 as P (parseOnly, count, decimal, space)
import Data.ByteString (ByteString)
import Control.Monad (void)

import Data.Either (fromRight)

import Prelude

-- * Part One

-- | p1
-- What is the sum of all metadata entries?
p1 :: ByteString -> Int
p1 = sumMeta . parseLiscence
  where
    sumMeta (N _ _ cs ms) = sum (fmap sumMeta cs) + sum ms

-- * Part Two

-- | p2
-- What is the value of the root node?
p2 :: ByteString -> Int
p2 = sumNode . parseLiscence
  where
    sumNode (N 0 _ _ ms) = sum ms
    sumNode (N _ _ cs ms) =
      sum [ sumNode (cs !! (m - 1))
          | m <- ms
          , m > 0 && m <= length cs
          ]

data Node =
  N Int -- * quantity of child nodes
    Int -- * quantity of metadata entries
    [Node] -- * child nodes
    [Int] -- * metadata entries
  deriving (Show)

parseLiscence :: ByteString -> Node
parseLiscence =
  fromRight (error "parseLiscence") . parseOnly parseNode . head . B8.lines
  where
    parseNode = do
      numChild <- decimal
      void space
      numMeta <- decimal
      children <- count numChild (space *> parseNode)
      meta <- count numMeta (space *> decimal)
      pure (N numChild numMeta children meta)
