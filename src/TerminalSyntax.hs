module TerminalSyntax where

import           Data.Char
import           Data.Universe.Class
import           Import                                 hiding (many, try,
                                                         (<|>))
import           Prelude                                (foldr1)
import           Text.Parsec                            (ParsecT, runParserT)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr     as P
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P


-- * Terminal's CLI language

data CmdName
    = Cat
    | Echo
    | Wc
    | Grep
    | Shell
    deriving (Show, Enum, Bounded, Universe, Finite)

data CLI
    = Cmd CmdName [String]
    | Export String String
    | Pipe CLI CLI
    deriving (Show)

data CmdContext
    = CmdContext
        { ccStdin :: String
        , ccArgs  :: [String]
        }

data CmdOutput =
    Success String
  | Failure String
  deriving (Eq, Show)


-- * Parsers

type EvalParser m = ParsecT String () m


varEvalParser :: HasEvalContext m => EvalParser m String
varEvalParser = concat <$> many p where
    p = p1 <|> p2
    p1 = lift . getEnv =<< string "$" *> (identifier <|> braces identifier)
    p2 = many1 $ noneOf "$"


evalVars :: HasEvalContext m => String -> m String
evalVars s = fromRight "" <$> runParserT varEvalParser () "" s


-- | Token parsers.
P.TokenParser{..} = P.makeTokenParser $ emptyDef
    { commentStart = ""
    , commentEnd = ""
    , commentLine = "#"
    , identStart = letter
    , identLetter = alphaNum
    , opStart = oneOf "|="
    , opLetter = oneOf "|="
    , reservedOpNames = ["|", "="]
    , reservedNames = ["cat", "export", "echo", "wc", "grep", "shell"]
    }


cmdNameParser :: Parser CmdName
cmdNameParser = foldr1 ((<|>) . try) (mkParser <$> universeF)
    where mkParser t = t <$ string (toLower <$> show t)


-- | Expression parser.
cliParser :: Parser CLI
cliParser = P.buildExpressionParser table term <?> "expression"
    where
        table =
            [ [Infix (reservedOp "|" >> return Pipe) AssocLeft]
            ]
        term =
                (Cmd <$> lexeme cmdNameParser <*> many argument)
            <|> (reserved "export" >> Export <$> (identifier <* reservedOp "=") <*> argument)


unquoted :: Parser String
unquoted = many1 $ satisfy ((&&) <$> not . isSpace <*> not . (`elem` ['|', '\\', '"']))


argument :: Parser String
argument = stringLiteral <|> lexeme unquoted
