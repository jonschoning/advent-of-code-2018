{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Criterion (nf, benchmark)
import Control.DeepSeq (NFData, rnf)
import qualified Data.ByteString.Char8 as B8
import qualified Day1 as D1
import qualified Day2 as D2

data AnyShow = forall a. (NFData a, Show a) => S a 
instance Show AnyShow where showsPrec p (S a) = showsPrec p a
instance NFData AnyShow where rnf (S a) = rnf a

main :: IO ()
main = do
  (cmd:day:part:inputfile:_) <- getArgs
  let dp = (day, part)
  input <- B8.readFile inputfile
  case cmd of
    "b" -> benchmark (nf (go dp) input)
    "p" -> print ((go dp) input)
    _   -> error "invalid cmd"
 where
  go :: (String, String) -> B8.ByteString -> AnyShow
  go = \case
    ("1" , "1") -> S . D1.p1
    ("1" , "2") -> S . D1.p2
    ("2" , "1") -> S . D2.p1
    _           -> error "invalid arguments"
