{-# LANGUAGE OverloadedStrings #-}

module Day1Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day1 as D1

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "input: file[input/day1.txt]" $ do
        input <- B8.readFile "input/day1.txt"
        D1.p1 input `shouldBe` 502

    describe "Part2" $ do
      it "input: file[input/day1.txt]" $ do
        input <- B8.readFile "input/day1.txt"
        D1.p2 input `shouldBe` 71961
