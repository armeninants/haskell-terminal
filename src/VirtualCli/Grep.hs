module VirtualCli.Grep where

import           Import         hiding (many)
-- import qualified System.IO as IO
import           TerminalSyntax
-- import qualified Options.Applicative.Simple as O
-- import qualified Options.Applicative.Help.Chunk as O
-- import qualified Options.Applicative.Types as O

data GrepOptions = GrepOptions
    { goA      :: Int
    , goB      :: Int
    , goRegExp :: String
    , goText   :: Maybe String -- ^ Nothing correspongs to stdin
    }

execGrep :: CmdContext -> RIO App CmdOutput
execGrep CmdContext{..} =
    return $ Failure "Not implemented yet."

-- catHandle :: Handle -> IO ()
-- catHandle h = do
--   eof <- IO.hIsEOF h
--   if eof
--   then
--     IO.hClose h
--   else do
--     IO.hGetLine h >>= IO.putStrLn
--     catHandle h
