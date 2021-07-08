module CLIParser (cliParser) where


import Data.Char
import Lexer
import RIO                                hiding (many, try, (<|>))
import TerminalSyntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr as P
import ToyPrograms


cliParser :: Parser Program
cliParser = P.buildExpressionParser table term <?> "expression"
    where
        table =
            [ [Infix (reservedOp "|" >> return (>=>)) AssocLeft]
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
        unknown -> \_ -> pThrowError $ "There is no command " ++ unknown


exportParser :: Parser Program
exportParser = do
    var <- reserved "export" *> identifier
    val <- reservedOp "=" *> argument
    return $ exportProgram [var, val]