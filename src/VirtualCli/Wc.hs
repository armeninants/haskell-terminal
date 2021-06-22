module VirtualCli.Wc where

import           Import          hiding (many)
import qualified System.IO       as IO
import qualified System.IO.Error as IO
import           TerminalSyntax


execWc :: CmdContext -> RIO App CmdOutput
execWc CmdContext{..} =
    if null ccArgs
    then return $ Success $ countWords ccStdin
    else liftIO $ IO.catchIOError
        (Success . unlines <$> forM ccArgs fileHandler)
        (return . Failure . show)
    where
        countWords =  show . length . words
        fileHandler = (fmap . fmap) countWords IO.readFile
