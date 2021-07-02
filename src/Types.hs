module Types where

import           RIO
import qualified RIO.Map                  as Map
import           System.Console.Haskeline


newtype App = App
    { env :: MVar (Map String String)
    }

newApp :: IO App
newApp = App <$> newMVar Map.empty

type AppMonad = ReaderT App (InputT IO)

runApp :: (MonadIO m) => App -> AppMonad a -> m a
runApp env (ReaderT f) = liftIO (runInputT defaultSettings $ f env)
