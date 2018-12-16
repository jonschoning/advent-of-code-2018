{-# LANGUAGE StrictData #-}


module Day14 where

import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace

import Prelude

-- * Part One
  
-- | p1
p1 :: ByteString -> String
p1 (B8.readInt -> Just (n, _)) =
  let x = n 
  in show n

-- | p2
-- What is the X,Y,size identifier of the square with the largest total power?
p2 :: ByteString -> String
p2 (B8.readInt -> Just (n, _)) =
  let x = 0
  in show n

-- * Util
