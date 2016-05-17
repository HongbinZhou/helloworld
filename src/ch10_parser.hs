
-- ref: http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html

import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64           -- imported from Data.Int
    } deriving (Show)


simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

-- NOTE:
-- Parse doesn't hide the fact that a parse state consists of a string and an
-- offset. It only hides the "plumbing" - the use of Either String (a,
-- ParseState) to represent the parsing state. ParseByte and ParseBytes below
-- still access and modify the parsing state

  .
newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
  }
