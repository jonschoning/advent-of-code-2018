module Day4Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day4 as D4

import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "sample" $ do
        let input = sampleInput
        D4.p1 input `shouldBe` 240
      it "input: file[input/day4.txt]" $ do
        input <- B8.readFile "input/day4.txt"
        D4.p1 input `shouldBe` 104764

    describe "Part2" $ do
      it "sample" $ do
        let input = sampleInput
        D4.p2 input `shouldBe` 4455
      it "input: file[input/day4.txt]" $ do
        input <- B8.readFile "input/day4.txt"
        D4.p2 input `shouldBe` 128617

sampleInput :: B8.ByteString
sampleInput = B8.unlines $
              ["[1518-11-01 00:00] Guard #10 begins shift"
              ,"[1518-11-01 00:05] falls asleep"
              ,"[1518-11-01 00:25] wakes up"
              ,"[1518-11-01 00:30] falls asleep"
              ,"[1518-11-01 00:55] wakes up"
              ,"[1518-11-01 23:58] Guard #99 begins shift"
              ,"[1518-11-02 00:40] falls asleep"
              ,"[1518-11-02 00:50] wakes up"
              ,"[1518-11-03 00:05] Guard #10 begins shift"
              ,"[1518-11-03 00:24] falls asleep"
              ,"[1518-11-03 00:29] wakes up"
              ,"[1518-11-04 00:02] Guard #99 begins shift"
              ,"[1518-11-04 00:36] falls asleep"
              ,"[1518-11-04 00:46] wakes up"
              ,"[1518-11-05 00:03] Guard #99 begins shift"
              ,"[1518-11-05 00:45] falls asleep"
              ,"[1518-11-05 00:55] wakes up"
              ]
