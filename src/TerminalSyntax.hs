{-# LANGUAGE TemplateHaskell #-}

module TerminalSyntax where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Data.Char
import           Data.Universe.Class
import           Prelude                                (foldr1)
import           RIO                                    hiding (many, try,
                                                         (<|>))
import qualified RIO.Text                               as T
import           Text.Parsec                            (ParsecT, runParserT)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr     as P
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P
import Lexer

------------------------------------------------------------
-- Program Syntax

data TerminalF next
    = TReadLine String (Maybe String -> next)
    | TParse String (Either String Program -> next)
    | TRun Program (Either String String -> next)
    | TPrint String next
    | TPrintError String next
    | TGetEnv String (String -> next)
    deriving (Functor)


type Terminal = Free TerminalF


data ProgramF next where
    PSafeIO :: IO a -> (a -> next) -> ProgramF next
    PGetEnv :: String -> (String -> next) -> ProgramF next
    PSetEnv :: String -> String -> (String -> next) -> ProgramF next
    PThrowError :: String -> (String -> next) -> ProgramF next


deriving instance Functor ProgramF


type ProgramM = Free ProgramF


type Program = String -> ProgramM String


makeFree ''TerminalF


makeFree ''ProgramF


terminalApp :: Terminal ()
terminalApp = fix $ \loop -> do
    cli <- evalVars . trim =<< tGetLine
    case cli of
        ":q" -> return ()
        "help" -> tHelp >> loop
        _ -> do
            mprog <- tParse cli
            either tPrintError tHandleProgram mprog
            loop


-- | Takes a cli input and returns the results of its execution.
terminalTestApp :: String -> Terminal (Either String String)
terminalTestApp = evalVars >=> tParse >=> either (return . Left) tRun


tGetLine :: Terminal String
tGetLine = tReadLine "haskell-terminal> " >>= maybe tGetLine return


tHandleProgram :: Program -> Terminal ()
tHandleProgram = tRun >=> either tPrintError tPrintLn


tHelp :: Terminal ()
tHelp = tPrint "This is help\n"


tPrintLn :: String -> Terminal ()
tPrintLn = tPrint . (++ "\n") . trimEnd


evalVars :: String -> Terminal String
evalVars s = fromRight "" <$> runParserT varEvalParser () "" s
    where
        varEvalParser :: ParsecT String () Terminal String
        varEvalParser = concat <$> many p where
            p = p1 <|> p2
            p1 = lift . tGetEnv =<< string "$" *> (identifier <|> braces identifier)
            p2 = many1 $ noneOf "$"

trimEnd :: String -> String
trimEnd = T.unpack . T.stripEnd . T.pack


trim :: String -> String
trim = T.unpack . T.strip . T.pack
