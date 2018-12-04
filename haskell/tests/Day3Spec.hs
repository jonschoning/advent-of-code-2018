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
      it "input: file[input/day3.txt]" $ do
        input <- B8.readFile "input/day3.txt"
        D3.p1 input `shouldBe` 0
