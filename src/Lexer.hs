module Lexer where


import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P


P.TokenParser{..} = P.makeTokenParser emptyDef
    { commentStart = ""
    , commentEnd = ""
    , commentLine = "#"
    , identStart = letter
    , identLetter = alphaNum
    , opStart = oneOf "|="
    , opLetter = oneOf "|="
    , reservedOpNames = ["|", "="]
    , reservedNames = ["export"]
    }
