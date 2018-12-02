{-# LANGUAGE BangPatterns #-}

module Day2 where

import qualified Data.ByteString.Char8 as B8

-- * Part One

-- | p1
p1 :: B8.ByteString -> Int
p1 input = 0


-- * Util
readLines :: B8.ByteString -> [B8.ByteString]
readLines = B8.lines
