module Day8Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day8 as D8

import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "sample" $ do
        let sampleInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2\n"
        D8.p1 sampleInput `shouldBe` 138
      -- it "input: file[input/day8.txt]" $ do
      --   input <- B8.readFile "input/day8.txt"
      --   D8.p1 input `shouldBe` 0

    -- describe "Part2" $ do
      -- it "sample" $ do
      --   let input = sampleInput
      --   D8.p2 input `shouldBe` 0
      -- it "input: file[input/day8.txt]" $ do
      --   input <- B8.readFile "input/day8.txt"
      --   D8.p2 input `shouldBe` 0
