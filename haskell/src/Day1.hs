{-# LANGUAGE BangPatterns #-}

module Day1 where

import qualified Data.ByteString.Char8 as B8
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Set as S

-- * Part One
p1 :: B8.ByteString -> Int
p1 = foldl' (+) 0 . readLines

p2 :: B8.ByteString -> Int
p2 = findFirstDuplicate . scanl' (+) 0 . cycle . readLines

findFirstDuplicate :: [Int] -> Int
findFirstDuplicate freqs = go S.empty freqs
  where
    go _ [] = error "not found"
    go seen (cur_freq:_) | S.member cur_freq seen = cur_freq
    go seen (cur_freq:rest) = go (S.insert cur_freq seen) rest

-- * Util
readLines :: B8.ByteString -> [Int]
readLines = fmap (fst . fromJust . B8.readInt) . B8.lines
