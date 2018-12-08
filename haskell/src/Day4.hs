{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Day4 where

import qualified Data.ByteString.Char8 as B8
import qualified Data.IntMap as M

import Control.Monad.State
import Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.IntMap (IntMap)
import Data.List (maximumBy, sort)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import Prelude

-- * Part One

-- | p1
-- Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?
-- What is the ID of the guard you chose multiplied by the minute you chose?
p1 :: ByteString -> Int
p1 (parseEvents -> events) =
  let freqs = asleepFreq events
      _id = fst $ maximumBy (comparing snd) (M.toList (fmap sum freqs))
      mn = fromJust $ fst . findFreqMinute <$> M.lookup _id freqs
  in _id * mn

-- * Part Two

-- | p2
-- Of all guards, which guard is most frequently asleep on the same minute?
-- What is the ID of the guard you chose multiplied by the minute you chose?
p2 :: ByteString -> Int
p2 (parseEvents -> events) =
  let xs = M.toList (fmap findFreqMinute (asleepFreq events))
      (_id, (mn, _)) = maximumBy (comparing (snd . snd)) xs
  in _id * mn

-- * Impl

type GuardId = Int
type Minute = Int
type Count = Int

data Event
  = Guard GuardId -- * id
  | S Minute -- * sleep
  | W Minute -- * wake
  deriving (Show)

findFreqMinute :: IntMap Count -> (Minute, Count)
findFreqMinute = maximumBy (comparing snd) . M.toList

asleepFreq :: [Event] -> IntMap (IntMap Count)
asleepFreq = evalState' (0, 0) . foldM go M.empty
  where
    go m (Guard _id) = put (_id, 0) *> pure m
    go m (S s_minute) = modify (\(_id, _) -> (_id, s_minute)) *> pure m
    go m (W w_minute) = do
      (_id, s_minute) <- get
      let new_freqs = M.fromList (fmap (, 1) [s_minute .. w_minute - 1])
      pure (M.insertWith (M.unionWith (+)) _id new_freqs m)

parseEvents :: ByteString -> [Event]
parseEvents = rights . fmap parseLine . sort . B8.lines
  where
    parseInt = P.decimal :: P.Parser Int
    parseLine line =
      (flip parseOnly) line $ do
        char '['
        s_minute <- P.takeWhile (/= ':') *> char ':' *> parseInt
        char ']'
        space
        event <-
          choice
            [ string "Guard #" *> fmap Guard parseInt
            , char 'f' *> pure (S s_minute) 
            , char 'w' *> pure (W s_minute) 
            ]
        pure $ event

evalState' :: s -> State s c -> c
evalState' = flip evalState
