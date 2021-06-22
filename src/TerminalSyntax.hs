module TerminalSyntax where

import           Data.Char
import           Data.Universe.Class
import           Import                        hiding (many)
import           Prelude                       (foldr1)
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec as P

-- * Terminal language

newtype CatOptions = CatOptions
  { coFiles :: [String]
  }

data CmdName =
    Cat
  | Export
  | Echo
  | Wc
  | Grep
  | Shell
  deriving (Show, Enum, Bounded, Universe, Finite)

type Stdin = String

type Argument = String

data Cmd = Cmd
    { cmdName :: CmdName
    , cmdArgs :: [Argument]
    } deriving (Show)

newtype CLI = CLI [Cmd]
  deriving (Show)

data CmdContext = CmdContext
    { ccStdin :: String
    , ccArgs  :: [Argument]
    }

data CmdOutput =
    Success String
  | Failure String
  deriving (Eq, Show)

-- * Parsers

cliParser :: Parser CLI
cliParser = CLI <$> (spaces *> sepBy cmdParser (string "|" <* spaces))

cmdParser :: Parser Cmd
cmdParser = do
    cmdName <- cmdNameParser
    spaces
    Cmd cmdName <$> argsParser

cmdNameParser :: Parser CmdName
cmdNameParser = foldr1 ((P.<|>) . P.try) (mkParser <$> universeF)
    where mkParser t = t <$ string (toLower <$> show t)

argsParser :: Parser [String]
argsParser = sepEndBy (concat <$> many1 (choice [parseUnquoted, parseQuoted])) (many1 space)

parseQuoted :: Parser String
parseQuoted = concat <$> betweenDoubleQuotes (many inner)
    where
        inner = fmap return (P.try nonEscape) P.<|> escape
        -- betweenSingleQuotes x = char '\'' *> x <* char '\''
        betweenDoubleQuotes x = char '"' *> x <* char '"'

parseUnquoted :: Parser String
parseUnquoted = many1 $ noneOf ['"', ' ', '|']

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf ['\\', '\"', '0', 'n', 'r', 'v', 't', 'b', 'f']
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf ['\\', '\"', '\0', '\n', '\r', '\v', '\t', '\b', '\f']
