module TerminalSemantics where

import           Import         hiding (many)
import qualified System.IO      as IO
import           TerminalSyntax
import           VirtualCli


execCli :: CLI -> AppMonad CmdOutput
execCli = go "" where
    go stdin' (Cmd name args) = execCmd name (CmdContext stdin' args)
    go stdin' (Export var val) = execExport (CmdContext stdin' [var, val])
    go stdin' (Pipe cmd1 cmd2) = do
        out <- go stdin' cmd1
        case out of
            Success str -> go str cmd2
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

