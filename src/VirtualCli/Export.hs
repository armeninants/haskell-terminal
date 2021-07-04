module VirtualCli.Export where

import Import         hiding (many)
import TerminalSyntax


execExport :: CmdContext -> AppMonad CmdOutput
execExport CmdContext{..} = case ccArgs of
    var:val:_ -> do
        setEnv var val
        return $ Success ""
    _ -> return $ Failure "Not enough arguments"
