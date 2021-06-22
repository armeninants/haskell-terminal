module VirtualCli.Export where

import           Import                        hiding (many)
import qualified RIO.Map                       as Map
import           TerminalSyntax
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec as P


execExport :: CmdContext -> RIO App CmdOutput
execExport CmdContext{..} = do
  let args = concat ccArgs
  case parse varValParser "" args of
    Left e -> return $ Failure $ show e
    Right (var, val) -> do
        env <- view $ to env
        modifyMVar_ env (return . Map.insert var val)
        return $ Success ""

interpolate :: Argument -> RIO App Argument
interpolate arg = do
    env <- view $ to env
    m <- readMVar env
    case parse (multiVarParser m) "" arg of
        Left _e    -> return arg
        Right arg' -> return arg'

-- * Parsers

identifier :: Parser String
identifier = do
    c  <- letter
    cs <- many (alphaNum P.<|> char '_')
    return (c:cs)

varParser :: Map String String -> Parser String
varParser m = do
    ident <- char '$' *> choice [identifier, between (char '{') (char '}') identifier]
    return $ Map.findWithDefault "" ident m

multiVarParser :: Map String String -> Parser Argument
multiVarParser m = concat <$> many (choice [varParser m, many1 $ noneOf ['$']])

varValParser :: Parser (String, String)
varValParser = do
    var <- identifier
    _ <- char '='
    val <- many anyChar
    return (var, val)
