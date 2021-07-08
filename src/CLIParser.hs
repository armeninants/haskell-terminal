module CLIParser (cliParser) where


import Data.Char
import Lexer
import RIO                                hiding (many, try, (<|>))
import TerminalSyntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr as P
import Text.Printf                        (printf)
import ToyPrograms


cliParser :: Parser Program
cliParser = P.buildExpressionParser table term <?> "command line instruction"
    where
        table =
            [ [Infix (reservedOp "|" >> return pipe) AssocLeft]
            ]
        term =  progParser <|> exportParser


unquoted :: Parser String
unquoted = many1 $ satisfy ((&&) <$> not . isSpace <*> not . (`elem` ['|', '\\', '"']))


argument :: Parser String
argument = stringLiteral <|> lexeme unquoted


progParser :: Parser Program
progParser = do
    cmd <- identifier
    args <- many argument
    return $ case cmd of
        "cat"   -> catProgram args
        "echo"  -> echoProgram args
        "wc"    -> wcProgram args
        "grep"  -> grepProgram args
        "shell" -> shellProgram args
        unknown -> \_ -> pThrowError $ printf "%s is unknown." unknown


exportParser :: Parser Program
exportParser = do
    var <- reserved "export" *> identifier
    val <- reservedOp "=" *> argument
    return $ exportProgram [var, val]


pipe :: Program -> Program -> Program
pipe = (>=>)
