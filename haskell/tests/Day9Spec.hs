
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
        D9.p1 sampleInput `shouldBe` 0
      -- it "input: file[input/day9.txt]" $ do
      --   input <- B9.readFile "input/day9.txt"
      --   D9.p1 input `shouldBe` 0

    -- describe "Part2" $ do
    --   it "sample" $ do
    --     D9.p2 sampleInput `shouldBe` 0
    --   it "input: file[input/day9.txt]" $ do
    --     input <- B9.readFile "input/day9.txt"
    --     D9.p2 input `shouldBe` 0

sampleInput :: B8.ByteString
sampleInput = ""
