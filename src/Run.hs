module Run where

import           Import
import           System.Console.Haskeline
import qualified System.IO                     as IO
import           TerminalSemantics
import           TerminalSyntax
import           Text.ParserCombinators.Parsec
import           VirtualCli.Export             (interpolate)


run :: AppMonad ()
run = do
    liftIO $ IO.hSetBuffering stdout NoBuffering
    forever $ do
        str <- getCmd
        str' <- interpolate str
        case parse cliParser "" str' of
            Left _err   -> liftIO $ IO.putStrLn "Invalid command."
            Right chain -> execCli chain >>= showCmdOut

getCmd :: AppMonad String
getCmd = fromMaybe "" <$> lift (getInputLine "haskell-terminal> ")
