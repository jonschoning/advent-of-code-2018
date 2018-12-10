module Day10Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day10 as D10

import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "sample" $ do
        D10.p1 sampleInput `shouldBe` 0
      -- it "input: file[input/day10.txt]" $ do
      --   input <- B8.readFile "input/day10.txt"
      --   D10.p1 input `shouldBe` 0

    -- describe "Part2" $ do
    --   it "input: file[input/day10.txt]" $ do
    --     input <- B8.readFile "input/day10.txt"
    --     D10.p2 input `shouldBe` 0

sampleInput :: B8.ByteString
sampleInput = "" 
