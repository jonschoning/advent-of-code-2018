
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Day14Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day14 as D14

import Data.List
import Prelude

main :: IO ()
main = hspec spec

assertTrue :: Expectation
assertTrue = 0 `shouldBe` 0

spec :: Spec
spec = do

    describe "Part1" $ do
      it "input: 9" $ do
        D14.p1 "9" `shouldBe` "5158916779"
      it "input: 5" $ do
        D14.p1 "5" `shouldBe` "0124515891"
      it "input: 18" $ do
        D14.p1 "18" `shouldBe` "9251071085"
      it "input: 2018" $ do
        D14.p1 "2018" `shouldBe` "5941429882"
      it "input: file[input/day14.txt]" $ do
        input <- B8.readFile "input/day14.txt"
        D14.p1 input `shouldBe` "1413131339"

    describe "Part2" $ do
      it "input: 51589" $ do
        D14.p2 "51589" `shouldBe` 9
      it "input: 92510" $ do
        D14.p2 "92510" `shouldBe` 18
      it "input: 59414" $ do
        D14.p2 "59414" `shouldBe` 2018
      -- it "input: file[input/day14.txt]" $ do
      --   input <- B8.readFile "input/day14.txt"
      --   D14.p2 input `shouldBe` 20254833
