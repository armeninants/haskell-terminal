module Run where

import           Import
import qualified System.IO                     as IO
import           TerminalSemantics
import           TerminalSyntax
import           Text.ParserCombinators.Parsec
import           VirtualCli.Export             (interpolate)


run :: RIO App ()
run = do
    liftIO $ IO.hSetBuffering stdout NoBuffering
    forever $ do
        str <- liftIO getCmd
        str' <- interpolate str
        case parse cliParser "" str' of
            Left _err   -> liftIO $ IO.putStrLn "Invalid command."
            Right chain -> execCli chain >>= showCmdOut

getCmd :: IO String
getCmd = do
    IO.putStr "haskell-terminal> "
    IO.getLine
