{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Day11Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day11 as D11

import Data.List
import Prelude

main :: IO ()
main = hspec spec

assertTrue :: Expectation
assertTrue = 0 `shouldBe` 0

spec :: Spec
spec = do

    describe "Part1" $ do
      it "sample1" $ do
        D11.powerAt 8 (3,5) `shouldBe` 4
      it "input: file[input/day11.txt]" $ do
        input <- B8.readFile "input/day11.txt"
        D11.p1 input `shouldBe` (20,62)

    -- describe "Part2" $ do
      -- it "sample1" $ do
      --   D11.p2 "18\n" `shouldBe` (90,269,16)
      -- it "input: file[input/day11.txt]" $ do
      --   input <- B8.readFile "input/day11.txt"
      --   D11.p2 input `shouldBe` (229,61,16)
