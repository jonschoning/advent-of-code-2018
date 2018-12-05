module Day5Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day5 as D5

import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "input: file[input/day5.txt]" $ do
        input <- B8.readFile "input/day5.txt"
        D5.p1 input `shouldBe` 0

    -- describe "Part2" $ do
    --   it "input: file[input/day5.txt]" $ do
    --     input <- B8.readFile "input/day5.txt"
    --     D5.p2 input `shouldBe` 128617
