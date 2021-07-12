module TerminalSemantics where

import           Conduit
import           Control.Monad.Free            (iterM)
import           Parser                        (cliParser)
import           RIO                           hiding (Proxy)
import qualified RIO.Map                       as Map
import           System.Console.Haskeline      (InputT, defaultSettings,
                                                getInputLine, runInputT)
import           System.IO                     (putStr, putStrLn)
import           TerminalSyntax
import           Text.ParserCombinators.Parsec (parse)


newtype SessionContext = SessionContext
    { senv :: MVar (Map String String)
    }


newtype ProgramContext = ProgramContext
    { penv :: MVar (Map String String)
    }


type TerminalIO = ReaderT SessionContext (InputT IO)


type ProgramMIO = RIO ProgramContext


type ProgramMIOPipe = ConduitT TData TData ProgramMIO


type ProgramMIOProducer = ConduitT () TData ProgramMIO


type ProgramMIOConsumer = ConduitT TData Void ProgramMIO


type ProgramMIOEffect = ConduitT () Void ProgramMIO


newSession :: IO SessionContext
newSession = SessionContext <$> newMVar Map.empty


newProgramContext :: MonadIO m => SessionContext -> m ProgramContext
newProgramContext SessionContext{..} = do
    return $ ProgramContext {penv = senv}


runTerminal :: (MonadIO m) => Terminal a -> m a
runTerminal terminalProg = liftIO $ do
    hSetBuffering stdout NoBuffering
    env <- newSession
    runInputT defaultSettings $
        runReaderT (iterM interpretTerminal terminalProg) env


leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)


runProgram :: Exception e => Program -> TerminalIO (Maybe e)
runProgram prog = do
    context <- newProgramContext =<< ask
    let progIO = interpretProgram prog stdoutC
    runRIO context $ leftToMaybe <$> try progIO


testConsumer :: (Monoid a, Monad m) => ConduitT a Void m a
testConsumer = fromMaybe mempty <$> await


runProgramSync :: Exception e => Program -> TerminalIO (Either e TData)
runProgramSync prog = do
    context <- newProgramContext =<< ask
    let progIO = interpretProgram prog testConsumer
    runRIO context (try progIO)


interpretTerminal :: TerminalF (TerminalIO next) -> TerminalIO next
interpretTerminal = \case
    TReadLine str next -> do
        lift (getInputLine str) >>= next

    TParse str next ->
        next $ mapLeft (TErrorStr . fromString . show) (parse cliParser "" str)

    TRun prog next ->
        runProgram prog >>= next

    TRunSync prog next ->
        runProgramSync prog >>= next

    TPrint str next -> do
        liftIO (putStr str)
        next

    TPrintError err next -> do
        liftIO . putStrLn $ "\x1F937 " ++ displayException err
        next

    TGetEnv var next -> do
        env <- view $ to senv
        m <- readMVar env
        next $ Map.findWithDefault "" var m


interpretProgramF :: ProgramF (ProgramMIOPipe next) -> ProgramMIOPipe next
interpretProgramF = \case
    PSafeIO io next -> liftIO (tryIO io) >>= either (throwString . displayException >=> next) next

    PGetEnv var next -> do
        env <- view $ to penv
        m <- readMVar env
        next $ Map.findWithDefault "" var m

    PSetEnv var val next -> do
        env <- view $ to penv
        liftIO $ modifyMVar_ env (return . Map.insert var val)
        next

    PThrowError str next -> throwIO str >> next

    PThrowStr str next -> throwIO (TErrorStr str) >> next

    PAwait next -> await >>= next

    PYield str next -> yield str >> next


interpretProgram :: Program -> ProgramMIOConsumer a -> ProgramMIO a
interpretProgram prog cons = runConduit (return () .| go prog .| cons) where
    go (Atomic p)   = iterM interpretProgramF p
    go (Pipe p1 p2) = go p1 .| go p2
