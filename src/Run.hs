module Run where

import Import hiding (many)
import qualified System.IO as IO
import Text.ParserCombinators.Parsec
import TerminalSyntax
import TerminalSemantics


run :: RIO App ()
run = do
  liftIO $ IO.hSetBuffering stdout NoBuffering
  forever $ do
    str <- liftIO getCmd
    case parse cliParser "" str of
      Left _err   -> liftIO $ IO.putStrLn "Invalid command."
      Right chain -> execCli chain

getCmd :: IO String
getCmd = do
  IO.putStr "haskell-terminal> "
  IO.getLine
