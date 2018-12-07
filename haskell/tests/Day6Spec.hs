module Day6Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day6 as D6

import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "sample" $ do
        let input = sampleInput
        D6.p1 input `shouldBe` 17
      it "input: file[input/day6.txt]" $ do
        input <- B8.readFile "input/day6.txt"
        D6.p1 input `shouldBe` 4171

    -- describe "Part2" $ do
    --   it "input: file[input/day6.txt]" $ do
    --     input <- B8.readFile "input/day6.txt"
    --     D6.p2 input `shouldBe` 0

sampleInput :: B8.ByteString
sampleInput = B8.unlines $
            [ "1, 1"
            , "1, 6"
            , "8, 3"
            , "3, 4"
            , "5, 5"
            , "8, 9"
            ]
