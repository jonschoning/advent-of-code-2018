
{-# LANGUAGE OverloadedStrings #-}

module Day2Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day2 as D2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "sample" $ do
        let input = B8.unlines $ B8.words "abcdef bababc abbcde abcccd aabcdd abcdee ababab"
        D2.p1 input `shouldBe` 12
      it "input: file[input/day2.txt]" $ do
        input <- B8.readFile "input/day2.txt"
        D2.p1 input `shouldBe` 8118

    describe "Part2" $ do
      it "sample" $ do
        let input = B8.unlines $ B8.words "abcde fghij klmno pqrst fguij axcye wvxyz"
        D2.p2 input `shouldBe` "fgij"
      it "input: file[input/day2.txt]" $ do
        input <- B8.readFile "input/day2.txt"
        D2.p2 input `shouldBe` "jbbenqtlaxhivmwyscjukztdp"

