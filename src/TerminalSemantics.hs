module TerminalSemantics where

import           Import         hiding (many)
import qualified System.IO      as IO
import           TerminalSyntax
import           VirtualCli


execCli :: CLI -> AppMonad CmdOutput
execCli = go "" where
    go _stdin' (CLI []) = return $ Success ""
    go stdin' (CLI (Cmd{..}:rest)) = do
        out <- execCmd cmdName (CmdContext stdin' cmdArgs)
        if null rest
        then return out
        else case out of
            Success str -> go str (CLI rest)
            Failure str -> return (Failure str)

showCmdOut :: CmdOutput -> AppMonad ()
showCmdOut (Success str) = liftIO $ IO.putStrLn str
showCmdOut (Failure str) = liftIO $ IO.putStrLn str

execCmd :: CmdName -> CmdContext -> AppMonad CmdOutput
execCmd cmdName = case cmdName of
    Cat    -> execCat
    Echo   -> execEcho
    Wc     -> execWc
    Grep   -> execGrep
    Shell  -> execShell
    Export -> execExport
