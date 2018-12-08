{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Day8 where

import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)

import Data.List
import Data.Either
import Debug.Trace

import Prelude

-- * Part One

-- | p1
-- What is the sum of all metadata entries?
p1 :: ByteString -> Int
p1 (parseLiscence -> license) =
  let sumMeta (N _ cs ms) = sum (fmap sumMeta cs) + sum ms
  in sumMeta license

-- * Part Two

-- | p2
p2 :: ByteString -> Int
p2 _ = 0

data Header =
  H Int -- * quantity of child nodes
    Int -- * quantity of metadata entries
  deriving (Show)

data Node =
  N Header -- * header
    [Node] -- * child nodes
    [Int] -- * metadata entries
  deriving (Show)

parseLiscence :: ByteString -> Node
parseLiscence = fromRight (error "e") . parseOnly parseNode . head . B8.lines
  where
    int = P.decimal :: P.Parser Int
    parseNode = do
      numChild <- int
      space
      numMeta <- int
      children <- count numChild (space *> parseNode)
      meta <- count numMeta (space *> int)
      pure (N (H numChild numMeta) children meta)
