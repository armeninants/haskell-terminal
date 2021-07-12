{-# LANGUAGE TemplateHaskell #-}

module TerminalSyntax where

import           Control.Monad.Free
import           Control.Monad.Free.TH         (makeFree)
import           Lexer                         (braces, identifier)
import           RIO                           hiding (Proxy (..), many, try,
                                                (<|>))
import qualified RIO.Text                      as T
import           Text.Parsec                   (runParserT)
import           Text.ParserCombinators.Parsec (many, many1, noneOf, string,
                                                (<|>))


newtype TError = TErrorStr ByteString
    deriving (Eq, Show)

instance Exception TError where
    displayException (TErrorStr b) = show b


type TData = ByteString


data TerminalF next where
    TReadLine   :: String -> (Maybe String -> next) -> TerminalF next
    TParse      :: String -> (Either TError Program -> next) -> TerminalF next
    TRun        :: Program -> (Maybe TError -> next) -> TerminalF next
    TRunSync    :: Program -> (Either TError ByteString -> next) -> TerminalF next
    TPrint      :: String -> next -> TerminalF next
    TPrintError :: TError -> next -> TerminalF next
    TGetEnv     :: String -> (String -> next) -> TerminalF next

deriving instance Functor TerminalF


type Terminal = Free TerminalF


data ProgramF next where
    PSafeIO     :: IO a -> (a -> next) -> ProgramF next
    PGetEnv     :: String -> (String -> next) -> ProgramF next
    PSetEnv     :: String -> String -> next -> ProgramF next
    PThrowError :: TError -> next -> ProgramF next
    PThrowStr   :: ByteString -> next -> ProgramF next
    PAwait      :: (Maybe ByteString -> next) -> ProgramF next
    PYield      :: ByteString -> next -> ProgramF next


deriving instance Functor ProgramF


type ProgramM = Free ProgramF


-- | @Program = Atomic (ProgramM ()) | Pipe Program Program@
data Program
    = Atomic (ProgramM ())
    | Pipe Program Program



makeFree ''TerminalF


makeFree ''ProgramF


terminalApp :: Terminal ()
terminalApp = do
    tPrint  "+-----------------------------------------------+\n\
            \|          Welcome to Haskell Terminal!         |\n\
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
terminalTestApp :: String -> Terminal (Either TError ByteString)
terminalTestApp = evalVars >=> tParse >=> either (return . Left) tRunSync


tGetLine :: Terminal String
tGetLine = do
    l <- fmap trim <$> tReadLine "\x03bb \x2794 "
    case l of
        Nothing  -> tGetLine
        Just ""  -> tGetLine
        Just str -> return str


tHandleProgram :: Program -> Terminal ()
tHandleProgram = tRun >=> maybe (return ()) tPrintError


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
