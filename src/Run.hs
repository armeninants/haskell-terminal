module Run where

import           Import
import           System.Console.Haskeline
import qualified System.IO                     as IO
import           TerminalSemantics
import           TerminalSyntax
import           Text.ParserCombinators.Parsec


run :: AppMonad ()
run = do
    liftIO $ IO.hSetBuffering stdout NoBuffering
    forever $ do
        inputStr <- evalVars =<< getCmd
        case parse cliParser "" inputStr of
            Left _err   -> liftIO $ IO.putStrLn "Invalid command."
            Right chain -> execCli chain >>= showCmdOut


getCmd :: AppMonad String
getCmd = fromMaybe "" <$> lift (getInputLine "haskell-terminal> ")
