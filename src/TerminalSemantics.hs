module TerminalSemantics where

import Import hiding (many)
import qualified System.IO as IO
import TerminalSyntax
import VirtualCli

execCli :: CLI -> RIO App ()
execCli = go "" where
    go _stdin' (CLI []) = return ()
    go stdin' (CLI (Cmd{..}:rest)) = do
        cmdArgs' <- interpolate' cmdArgs
        out <- execCmd cmdName (CmdContext stdin' cmdArgs')
        if null rest
        then showCmdOut out
        else case out of
            Success str -> go str (CLI rest)
            Failure str -> showCmdOut (Failure str)

showCmdOut :: CmdOutput -> RIO App ()
showCmdOut (Success str) = liftIO $ IO.putStrLn str
showCmdOut (Failure str) = liftIO $ IO.putStrLn str

execCmd :: CmdName -> CmdContext -> RIO App CmdOutput
execCmd cmdName = case cmdName of
    Cat -> execCat
    Echo -> execEcho
    Wc -> execWc
    Grep -> execGrep
    Shell -> execShell
    Export -> execExport
