module TerminalSemantics where

import           Conduit
import           Control.Monad.Free            (iterM)
import           Data.Conduit.Process
import           Parser                        (cliParser)
import           RIO                           hiding (Proxy)
import qualified RIO.Map                       as Map
import           System.Console.Haskeline      (InputT, defaultSettings,
                                                getInputLine, runInputT)
import           System.IO                     (putStr, putStrLn)
import           TerminalSyntax
import           Text.ParserCombinators.Parsec (parse)
import Text.Printf                        (printf)
import Prelude (head)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process.Typed as CT
import qualified System.Process.Typed as PT
import qualified Data.Conduit.Binary as CB

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
    let progIO = interpretProgram' prog stdoutC
    runRIO context $ leftToMaybe <$> try progIO


testConsumer :: (Monoid a, Monad m) => ConduitT a Void m a
testConsumer = fromMaybe mempty <$> await


runProgramSync :: Exception e => Program -> TerminalIO (Either e TData)
runProgramSync prog = do
    context <- newProgramContext =<< ask
    let progIO = interpretProgram' prog testConsumer
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


interpretProgram' :: Program -> ProgramMIOConsumer a -> ProgramMIO a
interpretProgram' prog cons =
    withUnliftIO $ \u -> do
        (sphs, cleanProcs, inputProcs, errorProcs, outProc) <- f (toProgram' prog) [] [] [] [] Nothing
        if null sphs
        then unliftIO u $ runConduit (outProc .| cons)
        else do
            let outProc' = Concurrently (unliftIO u $ runConduit (outProc .| cons))
                inputProcs' = map (\(ip, clp) -> Concurrently (unliftIO u (runConduit ip) `finally` clp)) inputProcs
                -- inputProcs' = map (\(ip, clp) -> Concurrently (unliftIO u (runConduit ip))) inputProcs
                -- inputProcsJoint = mconcat inputProcs'
                inputProcsJoint = head inputProcs'
                errorProcs' = map (Concurrently . unliftIO u . runConduit) errorProcs
                -- errorProcsJoint = asum errorProcs'
                errorProcsJoint = head errorProcs'
                cleanProc = foldl' (>>) (pure ()) cleanProcs
            (_, resStdout, _resStderr) <-
                runConcurrently (
                    (,,)
                    <$> inputProcsJoint
                    <*> outProc'
                    <*> errorProcsJoint)
                `finally` cleanProc
                `onException` mapM_ terminateStreamingProcess sphs
            mapM_ waitForStreamingProcess sphs
            return resStdout

    -- where
f   :: Program'
    -> [StreamingProcessHandle]
    -> [IO ()]
    -> [(ProgramMIOEffect (), IO ())]
    -> [ProgramMIOEffect TError]
    -> Maybe (ProgramMIOProducer ())
    -> IO ([StreamingProcessHandle], [IO ()], [(ProgramMIOEffect (), IO ())], [ProgramMIOEffect TError], ProgramMIOProducer ())

f prog' sphs cleanProcs inputProcs errProcs mOut = case prog' of
    Program' (ps, Just (cmd, prog1)) -> do
        (  (sinkStdin :: ProgramMIOConsumer (), closeStdin)
        -- (  stdin_hdl :: Handle
            , (sourceStdout :: ProgramMIOProducer (), closeStdout)
            , (sourceStderr :: ProgramMIOProducer (), closeStderr)
            ,  sph) <- streamingProcess (shell cmd)

        -- let sinkStdin = sinkHandle stdin_hdl
            -- closeStdin = hClose stdin_hdl

        let errProc = TErrorStr <$> (sourceStderr .| testConsumer)
            firstProducer = fromMaybe (return ()) mOut
            inputProc = (mkProc firstProducer ps .| sinkStdin, closeStdin)
            -- inputProc = (yield "vacho" .| sinkStdin, closeStdin)

        f   prog1
            (sphs ++ [sph])
            (cleanProcs ++ [closeStdout, closeStderr])
            (inputProcs ++ [inputProc])
            (errProcs ++ [errProc])
            (Just sourceStdout)

    Program' (ps, Nothing) -> do
        let firstProducer = fromMaybe (return ()) mOut
            outProc = mkProc firstProducer ps
        return (sphs, cleanProcs, inputProcs, errProcs, outProc)

mkProc :: ProgramMIOProducer () -> [ProgramM ()] -> ProgramMIOProducer ()
mkProc = foldl' $ \initProd p -> initProd .| iterM interpretProgramF p

terminateStreamingProcess :: MonadIO m => StreamingProcessHandle -> m ()
terminateStreamingProcess = liftIO . terminateProcess . streamingProcessHandleRaw

------------------------------------------------------
-- Testing

test1 :: IO ([StreamingProcessHandle], [IO ()], [(ProgramMIOEffect (), IO ())], [ProgramMIOEffect TError], ProgramMIOProducer ())
test1 = do
    let cmd = "echo hello | shell cat"
    let x = parse cliParser "" cmd
    either (error "something went wrong") go x
    where
        go prog = do
            let prog' = toProgram' prog
            putStrLn $ pr prog'
            f (toProgram' prog) [] [] [] [] Nothing


test2 :: IO ()
test2 = do
    (e, a, b) <- sourceProcessWithStreams (shell "cat") (CC.yieldMany ["Hello", "World"]) CL.consume CL.consume
    printf "exit code %s\n" $ show e
    printf "out %s\n" $ show a
    printf "error %s\n" $ show b
    -- return undefined


test3 :: IO ()
test3 = do
    let stdin_sink = CT.createSink :: CT.StreamSpec 'CT.STInput (ConduitM ByteString Void IO ())
        pc = PT.setStdin stdin_sink (PT.shell "cat")
    CT.withLoggedProcess_ pc $ \p -> do
        let in_ = CT.getStdin p
        let proc1 = runConduit (CT.getStdout p .| stdoutC) <* CT.waitExitCode p
        let proc2 = runConduit (yield "Hello" .| in_)
        proc2
        proc1

-- | An aleternative representation of the program
-- newtype Program' = Program' ([ProgramM ()], Maybe (String, Program'))


pr :: Program' -> String
pr (Program' (l, Nothing)) = printf "([%d], Nothing)" (length l)
pr (Program' (l, Just (cmd, p))) = printf "([%d], Just (%s, %s) )" (length l) cmd (pr p)
