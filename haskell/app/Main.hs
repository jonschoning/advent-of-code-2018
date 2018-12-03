import System.Environment (getArgs)
import Criterion (benchmark, nf, nfIO)
import Control.DeepSeq (NFData, rnf)
import qualified Data.ByteString.Char8 as B8
import qualified Day1 as D1
import qualified Day2 as D2
-- import Options.Generic
import Prelude

data AnyShowNF = forall a. (NFData a, Show a) => S a 
instance Show AnyShowNF where showsPrec p (S a) = showsPrec p a
instance NFData AnyShowNF where rnf (S a) = rnf a


data DP =
  DP !String -- * Day
     !String -- * Part

main :: IO ()
main = do
  (cmd:day:part:inputfile:_) <- getArgs
  let goDP = go (DP day part)
      readInput = B8.readFile inputfile
      goIO = pure . goDP =<< readInput
  case cmd of
    "p" -> print =<< goIO
    "b" -> benchmark . nf goDP =<< readInput
    "bio" -> benchmark (nfIO goIO)
    _   -> error "invalid cmd"
 where
  go :: DP -> B8.ByteString -> AnyShowNF
  go = \case
    DP "1" "1" -> S . D1.p1
    DP "1" "2" -> S . D1.p2
    DP "2" "1" -> S . D2.p1
    DP "2" "2" -> S . D2.p2
    _           -> error "invalid arguments"
