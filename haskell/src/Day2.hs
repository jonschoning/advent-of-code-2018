module Day2 where

import qualified Data.ByteString.Char8 as B8
import Data.List
import Data.Monoid
import Data.Maybe
import Prelude

-- * Part One

-- | p1
-- What is the checksum for your list of box IDs?
p1 :: B8.ByteString -> Int
p1 = checksum . B8.lines

checksum :: [B8.ByteString] -> Int
checksum = result . foldl' go (0,0)
  where
    result (count2, count3) = count2 * count3
    go (!count2, !count3) cur = do
      let occur = fmap B8.length $ B8.group $ B8.sort cur
          has2 = if elem 2 occur then 1 else 0
          has3 = if elem 3 occur then 1 else 0
      (count2 + has2, count3 + has3)

-- * Part Two
data B8Pair = B8Pair !B8.ByteString !B8.ByteString

-- | p2
-- The boxes will have IDs which differ by exactly one character at the same position in both strings
p2 :: B8.ByteString -> B8.ByteString
p2 input =
  let pairs = concat [map (B8Pair x) xrest | (x:xrest) <- tails (B8.lines input)]
  in findBox pairs

findBox :: [B8Pair] -> B8.ByteString
findBox = fromJust . getFirst . foldMap go
  where
    go (B8Pair a b) =
      First $
      let select [i] =
            let (s, t) = B8.splitAt i a
            in Just (s <> B8.drop 1 t)
          select _ = Nothing
      in select (findIndices id (B8.zipWith (/=) a b))
