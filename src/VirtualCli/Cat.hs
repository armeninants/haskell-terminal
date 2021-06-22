module VirtualCli.Cat where

import           Import
import qualified System.IO       as IO
import qualified System.IO.Error as IO
import           TerminalSyntax
-- import qualified Options.Applicative.Simple as O
-- import qualified Options.Applicative.Help.Chunk as O
-- import qualified Options.Applicative.Types as O

execCat :: CmdContext -> RIO App CmdOutput
execCat CmdContext{..} =
    if null ccArgs
    then return $ Success ccStdin
    else liftIO $ IO.catchIOError
        (Success . unlines <$> forM ccArgs IO.readFile)
        (return . Failure . show)

-- catOptsParser :: O.Parser VirtualCli.CatOptions
-- catOptsParser = VirtualCli.CatOptions <$> (O.many . O.strArgument . O.help) "Files"
