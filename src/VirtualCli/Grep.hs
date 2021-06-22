module VirtualCli.Grep where

import           Import
import           Options.Applicative.Simple hiding (Failure, Success)
import qualified Options.Applicative.Simple as O
import qualified System.IO                  as IO
import           TerminalSyntax
import           Text.Regex.TDFA


data GrepOptions = GrepOptions
    { goA      :: Int
    , goB      :: Int
    , goRegExp :: String
    , goFiles  :: [String]
    } deriving (Show)

execGrep :: CmdContext -> RIO App CmdOutput
execGrep CmdContext{..} = do
    let resp = execParserPure defaultPrefs (info (grepOptsParser <**> helper) mempty) ccArgs
    case resp of
        O.Success grepOpts -> runGrep grepOpts
        O.Failure flr -> do
            let (flrStr, _) = O.renderFailure flr "grep"
            return $ Failure flrStr
        _ -> return $ Failure "Options are incorrect."
    where
        runGrep GrepOptions{..} = do
            text <- if null goFiles then return [ccStdin] else liftIO $ forM goFiles IO.readFile
            let matched = filter (=~ goRegExp) $ concatMap lines text
            return $ Success $ unlines matched

grepOptsParser :: Parser GrepOptions
grepOptsParser = GrepOptions
    <$> option auto
        (   short 'A'
         <> help "After context"
         <> value 0
        )
    <*> option auto
        (   short 'B'
         <> help "Before context"
         <> value 0
        )
    <*> strArgument (metavar "PATTERN")
    <*> some (strArgument (metavar "FILES..."))
