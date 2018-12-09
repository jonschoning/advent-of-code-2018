
module Day9Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day9 as D9

import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "sample" $ do
        D9.p1 sampleInput `shouldBe` 32  -- player 5
      it "input: file[input/day9.txt]" $ do
        input <- B8.readFile "input/day9.txt"
        D9.p1 input `shouldBe` 385820

    describe "Part2" $ do
      it "input: file[input/day9.txt]" $ do
        input <- B8.readFile "input/day9.txt"
        D9.p2 input `shouldBe` 3156297594

sampleInput :: B8.ByteString
sampleInput = "9 players; last marble is worth 25 points\n" 
