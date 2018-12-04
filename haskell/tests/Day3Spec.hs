module Day3Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day3 as D3

import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "sample" $ do
        let input = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
        D3.p1 input `shouldBe` 4
      it "input: file[input/day3.txt]" $ do
        input <- B8.readFile "input/day3.txt"
        D3.p1 input `shouldBe` 112378

    describe "Part2" $ do
      it "sample" $ do
        let input = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
        D3.p2 input `shouldBe` "3"
      it "input: file[input/day3.txt]" $ do
        input <- B8.readFile "input/day3.txt"
        D3.p2 input `shouldBe` "603"
