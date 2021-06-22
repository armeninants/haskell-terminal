module VirtualCli.Grep where

import           Import
import RIO.List
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
            x <- concat <$> forM text runGrep_
            return $ Success $ intercalate sep x where
                sep = if goA + goB > 0 then replicate 2 '-' ++ "\n" else "" 
                runGrep_ txt = return withContext where
                    ls = lines txt
                    matched = filter ((=~ goRegExp) . fst) $ zip ls [0..]
                    withContext = map (\x -> unlines $ takeLines goB goA (snd x) ls) matched
        takeLines b a i l = slice (max 0 $ i-b) (min (length l - 1) $ i+a) l

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
    <*> many (strArgument (metavar "FILES..."))
