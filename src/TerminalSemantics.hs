module TerminalSemantics where

import           Import         hiding (many)
import qualified System.IO      as IO
import           TerminalSyntax
import           VirtualCli


execCli :: CLI -> RIO App CmdOutput
execCli = go "" where
    go _stdin' (CLI []) = return $ Success ""
    go stdin' (CLI (Cmd{..}:rest)) = do
        out <- execCmd cmdName (CmdContext stdin' cmdArgs)
        if null rest
        then return out
        else case out of
            Success str -> go str (CLI rest)
            Failure str -> return (Failure str)

showCmdOut :: CmdOutput -> RIO App ()
showCmdOut (Success str) = liftIO $ IO.putStrLn str
showCmdOut (Failure str) = liftIO $ IO.putStrLn str

execCmd :: CmdName -> CmdContext -> RIO App CmdOutput
execCmd cmdName = case cmdName of
    Cat    -> execCat
    Echo   -> execEcho
    Wc     -> execWc
    Grep   -> execGrep
    Shell  -> execShell
    Export -> execExport
