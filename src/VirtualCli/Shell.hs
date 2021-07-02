module VirtualCli.Shell where

import           Import          hiding (many)
import qualified System.IO.Error as IO
import           System.Process  (readProcess)
import           TerminalSyntax


execShell :: CmdContext -> AppMonad CmdOutput
execShell CmdContext{..} = do
    liftIO $ IO.catchIOError
        (Success <$> readProcess "sh" ["-c", unwords ccArgs] "")
        (return . Failure . show)
