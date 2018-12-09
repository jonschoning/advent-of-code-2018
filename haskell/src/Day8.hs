{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Day8 where

import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)

import Data.List
import Data.Either
import Data.Function

import Prelude

-- * Part One

-- | p1
-- What is the sum of all metadata entries?
p1 :: ByteString -> Int
p1 (parseLiscence -> license) =
  let sumMeta (N _ _ cs ms) = sum (fmap sumMeta cs) + sum ms
  in sumMeta license

-- * Part Two

-- | p2
p2 :: ByteString -> Int
p2 (parseLiscence -> license) = sumNode license
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
parseLiscence = fromRight (error "e") . parseOnly parseNode . head . B8.lines
  where
    parseNode = do
      numChild <- decimal
      space
      numMeta <- decimal
      children <- count numChild (space *> parseNode)
      meta <- count numMeta (space *> decimal)
      pure (N numChild numMeta children meta)
