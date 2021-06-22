module VirtualCli.Echo where

import           Import
import           TerminalSyntax

execEcho :: CmdContext -> RIO App CmdOutput
execEcho CmdContext{..} = do
    if null ccArgs
    then return $ Success ccStdin
    else return $ Success $ unwords ccArgs
