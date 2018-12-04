import Control.DeepSeq (NFData, rnf)
import Criterion (benchmark, nf, nfIO)
import Options.Applicative ( Parser, argument, auto, customExecParser, fullDesc, str
                           , help, helper, info, metavar, prefs, showHelpOnError )
import qualified Data.ByteString.Char8 as B8

import qualified Day1 as D1
import qualified Day2 as D2

import Prelude

data Args =
  Args !String -- * Mode
       !Int -- * Day
       !Int -- * Part
       !String -- * Inputfile
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
