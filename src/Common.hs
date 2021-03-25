{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common where

import Database.MongoDB
import Types
import Control.Monad.Trans
import Data.Aeson
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.Except
import Servant


type App = ReaderT AppEnv Handler


runDb :: Action IO a -> AppM a
runDb a = do
  -- dbConf <- dbConfig <$> ask
  pipe <- liftIO $ connect (host "127.0.0.1")
  -- Have to check if it isn't authorized
  -- _ <- lift . lift $ access pipe master (dbName dbConf) $ auth (dbUser dbConf) (dbPassword dbConf)
  result <- lift $ access pipe master "tracker" a
  liftIO $ close pipe
  return result

data DbConfig =
  DbConfig { dbHostname :: String
           , dbName     :: Text
           , dbUser     :: Text
           , dbPassword :: Text }
           deriving (Show)

instance FromJSON DbConfig where
  parseJSON = withObject "dbConf" $ \o -> do
    dbHostname <- o .: "host"
    dbName     <- o .: "database"
    dbUser     <- o .: "username"
    dbPassword <- o .: "password"
    return DbConfig{..}

data AppEnv =
  AppEnv { dbConfig :: DbConfig
         , logPath  :: String }

instance FromJSON AppEnv where
  parseJSON = withObject "config" $ \o -> do
    logPath    <- o      .: "logPath"
    dbConf     <- o      .: "dbConfig"
    dbHostname <- dbConf .: "host"
    dbName     <- dbConf .: "database"
    dbUser     <- dbConf .: "username"
    dbPassword <- dbConf .: "password"
    let dbConfig = DbConfig {..}
    return (AppEnv dbConfig logPath)
