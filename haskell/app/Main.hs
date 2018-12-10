{-# LANGUAGE StrictData #-}

import Control.DeepSeq (NFData, rnf)
import Criterion (benchmark, nf, nfIO)
import Options.Applicative ( Parser, argument, auto, customExecParser, fullDesc, str
                           , help, helper, info, metavar, prefs, showHelpOnError )
import qualified Data.ByteString.Char8 as B8

import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
-- import qualified Day7 as D7
import qualified Day8 as D8
import qualified Day9 as D9
import qualified Day10 as D10

import Prelude

data Args =
  Args String -- * Mode
       Int -- * Day
       Int -- * Part
       String -- * Inputfile
  deriving (Show)

main :: IO ()
main = do
  args@(Args mode _ _ inputfile) <- parseArgs
  let goArgs = go args
      readInput = B8.readFile inputfile
      goIO = pure . goArgs =<< readInput
  case mode of
    "p" -> print =<< goIO
    "b" -> benchmark . nf goArgs =<< readInput
    "bio" -> benchmark (nfIO goIO)
    _   -> error "invalid mode"
 where

  go =
    \case
      Args _ 1 1 _ -> S . D1.p1
      Args _ 1 2 _ -> S . D1.p2
      Args _ 2 1 _ -> S . D2.p1
      Args _ 2 2 _ -> S . D2.p2
      Args _ 3 1 _ -> S . D3.p1
      Args _ 3 2 _ -> S . D3.p2
      Args _ 4 1 _ -> S . D4.p1
      Args _ 4 2 _ -> S . D4.p2
      Args _ 5 1 _ -> S . D5.p1
      Args _ 5 2 _ -> S . D5.p2
      Args _ 6 1 _ -> S . D6.p1
      Args _ 6 2 _ -> S . D6.p2
      -- Args _ 7 1 _ -> S . D7.p1
      -- Args _ 7 2 _ -> S . D7.p2
      Args _ 8 1 _ -> S . D8.p1
      Args _ 8 2 _ -> S . D8.p2
      Args _ 9 1 _ -> S . D9.p1
      Args _ 9 2 _ -> S . D9.p2
      Args _ 10 1 _ -> S . D10.p1
      Args _ 10 2 _ -> S . D10.p2
      _ -> error "invalid day or part"

  parseArgs =
    customExecParser
      (prefs showHelpOnError)
      (info (helper <*> argParser) fullDesc)

argParser :: Parser Args
argParser = Args
  <$> argument str (metavar "mode" <> help "p | b | bio (print | bench | benchio)")
  <*> argument auto (metavar "day" <> help "(integer)")
  <*> argument auto (metavar "part" <> help "(integer)")
  <*> argument str (metavar "inputfile" <> help "(filepath)")

data AnyShowNF = forall a. (NFData a, Show a) => S a 
instance Show AnyShowNF where showsPrec p (S a) = showsPrec p a
instance NFData AnyShowNF where rnf (S a) = rnf a
