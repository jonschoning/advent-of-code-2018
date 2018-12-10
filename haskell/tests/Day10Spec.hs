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
        D10.p1 sampleInput `shouldBe` ""
      -- it "input: file[input/day10.txt]" $ do
      --   input <- B8.readFile "input/day10.txt"
      --   D10.p1 input `shouldBe` 0

    -- describe "Part2" $ do
    --   it "input: file[input/day10.txt]" $ do
    --     input <- B8.readFile "input/day10.txt"
    --     D10.p2 input `shouldBe` 0

sampleInput :: B8.ByteString
sampleInput = B8.unlines
  [ "position=< 7,  0> velocity=<-1,  0>" 
  , "position=< 3, -2> velocity=<-1,  1>"
  , "position=< 6, 10> velocity=<-2, -1>"
  , "position=< 2, -4> velocity=< 2,  2>"
  , "position=<-6, 10> velocity=< 2, -2>"
  , "position=< 1,  8> velocity=< 1, -1>"
  , "position=< 1,  7> velocity=< 1,  0>"
  , "position=<-3, 11> velocity=< 1, -2>"
  , "position=< 7,  6> velocity=<-1, -1>"
  , "position=<-2,  3> velocity=< 1,  0>"
  , "position=<-4,  3> velocity=< 2,  0>"
  , "position=<10, -3> velocity=<-1,  1>"
  , "position=< 5, 11> velocity=< 1, -2>"
  , "position=< 4,  7> velocity=< 0, -1>"
  , "position=< 8, -2> velocity=< 0,  1>"
  , "position=<15,  0> velocity=<-2,  0>"
  , "position=< 1,  6> velocity=< 1,  0>"
  , "position=< 8,  9> velocity=< 0, -1>"
  , "position=< 3,  3> velocity=<-1,  1>"
  , "position=< 0,  5> velocity=< 0, -1>"
  , "position=<-2,  2> velocity=< 2,  0>"
  , "position=< 5, -2> velocity=< 1,  2>"
  , "position=< 1,  4> velocity=< 2,  1>"
  , "position=<-2,  7> velocity=< 2, -2>"
  , "position=< 3,  6> velocity=<-1, -1>"
  , "position=< 5,  0> velocity=< 1,  0>"
  , "position=<-6,  0> velocity=< 2,  0>"
  , "position=< 5,  9> velocity=< 1, -2>"
  , "position=<14,  7> velocity=<-2,  0>"
  , "position=<-3,  6> velocity=< 2, -1>" 
  ]
