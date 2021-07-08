{-# LANGUAGE TemplateHaskell #-}

module TerminalSyntax where

import           Control.Monad.Free
import           Control.Monad.Free.TH         (makeFree)
import           Lexer                         (braces, identifier)
import           RIO                           hiding (many, try, (<|>))
import qualified RIO.Text                      as T
import           Text.Parsec                   (runParserT)
import           Text.ParserCombinators.Parsec (many, many1, noneOf, string,
                                                (<|>))


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


-- | @String -> ProgramM String@
type Program = String -> ProgramM String


makeFree ''TerminalF


makeFree ''ProgramF


terminalApp :: Terminal ()
terminalApp = do
    tPrint  "+-----------------------------------------------+\n\
            \|      \x1F44B  Welcome to Haskell Terminal!         |\n\
            \| Type `help` for instructions or `:q` to quit. |\n\
            \|-----------------------------------------------+\n"
    fix $ \loop -> do
        cli <- evalVars =<< tGetLine
        case cli of
            ":q"   -> tPrintLn "bye \x1F44B"
            "help" -> tHelp >> loop
            _ -> do
                mprog <- tParse cli
                either tPrintError tHandleProgram mprog
                loop


-- | Takes a cli input and returns the results of its execution.
terminalTestApp :: String -> Terminal (Either String String)
terminalTestApp = evalVars >=> tParse >=> either (return . Left) tRun


tGetLine :: Terminal String
tGetLine = do
    l <- fmap trim <$> tReadLine "\x03bb \x2794 "
    case l of
        Nothing  -> tGetLine
        Just ""  -> tGetLine
        Just str -> return str


tHandleProgram :: Program -> Terminal ()
tHandleProgram = tRun >=> either tPrintError tPrintLn


tHelp :: Terminal ()
tHelp = tPrint
    "\nThe supported commands are: cat, echo, grep, wc, shell.\n\
    \Use --help pragma for more info, e.g. \n\
    \  grep --help\n\n\
    \This terminal supports local environment variables.\n\
    \  export varname=\"some value\"\n\
    \  echo $varname\n\
    \will print \"some value\".\n\n\
    \Programs can be combined with the pipe operator, e.g.\n\
    \  echo \"one\\ntwo\\nthree\\nfour\" | grep th\n\
    \will print \"three\".\n\n\
    \To quit the terminal, type `:q`.\n\n"


tPrintLn :: String -> Terminal ()
tPrintLn = tPrint . (\s -> if null s then s else s ++ "\n") . trimEnd


evalVars :: String -> Terminal String
evalVars s = fromRight "gago" <$> runParserT p () "" s
    where
        p = concat <$> many (p1 <|> p2)
        p1 = lift . tGetEnv =<< string "$" *> (identifier <|> braces identifier)
        p2 = many1 $ noneOf "$"


trimEnd :: String -> String
trimEnd = T.unpack . T.stripEnd . T.pack


trim :: String -> String
trim = T.unpack . T.strip . T.pack
