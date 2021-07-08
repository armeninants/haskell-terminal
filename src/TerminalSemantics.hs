module TerminalSemantics where

import           Control.Monad.Except          (ExceptT, runExceptT, throwError)
import           Control.Monad.Free
import RIO                        hiding (many, (<|>))
import qualified RIO.Map                       as Map
import           System.Console.ANSI
import           System.Console.Haskeline
import           System.IO                     (putStr, putStrLn)
-- import qualified System.IO                as IO
import           TerminalSyntax
import           Text.ParserCombinators.Parsec
import           ToyPrograms


------------------------------------------------------------
-- CLI Semantics

toProgram :: CLI -> Program
toProgram = \case
    Cmd name args -> case name of
        Cat   -> catProgram args
        Echo  -> echoProgram args
        Wc    -> wcProgram args
        Grep  -> grepProgram args
        Shell -> shellProgram args

    Export var val -> exportProgram [var, val]

    Pipe cmd1 cmd2 -> toProgram cmd1 >=> toProgram cmd2


------------------------------------------------------------
-- Program Semantics

newtype SessionContext = SessionContext
    { senv :: MVar (Map String String)
    }


data ProgramContext
    = ProgramContext
        { penv :: MVar (Map String String)
        , pout :: MVar String
        }


type TerminalIO = ReaderT SessionContext (InputT IO)


type ProgramMIO = ExceptT String (ReaderT ProgramContext IO)


newSession :: IO SessionContext
newSession = SessionContext <$> newMVar Map.empty


newProgramContext :: MonadIO m => SessionContext -> m ProgramContext
newProgramContext SessionContext{..} = do
    outM <- newMVar ""
    return $ ProgramContext {penv = senv, pout = outM}


runTerminal :: (MonadIO m) => Terminal a -> m a
runTerminal terminalProg = liftIO $ do
    hSetBuffering stdout NoBuffering
    env <- newSession
    runInputT defaultSettings $
        runReaderT (iterM interpretTerminal terminalProg) env


runProgram :: Program -> String -> TerminalIO (Either String String)
runProgram prog input = do
    context <- newProgramContext =<< ask
    let progIO = iterM interpretProgram (prog input)
    liftIO $ runReaderT (runExceptT progIO) context


interpretTerminal :: TerminalF (TerminalIO next) -> TerminalIO next
interpretTerminal = \case
    TReadLine str next -> lift (getInputLine str) >>= next

    TParse str next -> do
        case parse cliParser "" str of
            Left _err -> next $ Left "Invalid command."
            Right cli -> next $ Right $ toProgram cli

    TRun prog next -> runProgram prog "" >>= next

    TPrint str next -> do
        liftIO $ putStr str
        next

    TPrintError str next -> do
        liftIO $ do
            setSGR [SetColor Foreground Dull Red]
            putStr "Error: "
            setSGR [Reset]
            putStrLn str
        next

    TQuit _ -> exitSuccess

    TGetEnv var next -> do
        env <- view $ to senv
        m <- readMVar env
        next $ Map.findWithDefault "" var m


interpretProgram :: ProgramF (ProgramMIO next) -> ProgramMIO next
interpretProgram = \case
    PSafeIO io next -> liftIO (tryIO io) >>= either (throwError . show >=> next) next

    PGetEnv var next -> do
        env <- view $ to penv
        m <- readMVar env
        next $ Map.findWithDefault "" var m

    PSetEnv var val next -> do
        env <- view $ to penv
        liftIO $ modifyMVar_ env (return . Map.insert var val)
        next ""

    PThrowError str next -> throwError str >> next ""

    PPrint str next -> do
        out <- view $ to pout
        liftIO $ modifyMVar_ out (return . (++ str))
        next ""

    PReturn next -> do
        outM <- view $ to pout
        out <- readMVar outM
        liftIO $ modifyMVar_ outM (const $ return "")
        next out
