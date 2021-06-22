module Types where

import RIO
import qualified RIO.Map as Map

newtype App = App
    { env :: MVar (Map String String)
    }

newApp :: IO App
newApp = App <$> newMVar Map.empty