{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.Aeson
import           Database.MongoDB
import           Control.Exception.Base
import           Types

databasename :: Database
databasename = "tracker"

runDb :: Action IO a -> AppM a
runDb action = do
  dbConf <- asks dbConfig
  pipe <- liftIO $ connect (host (dbHostname dbConf))
  -- Have to check if it isn't authorized
  -- _ <- lift . lift $ access pipe master (dbName dbConf) $ auth (dbUser dbConf) (dbPassword dbConf)
  result <- liftIO $ access pipe master (dbName dbConf) action
  liftIO $ close pipe
  return result

runDb_ :: Action IO a -> DbConfig -> IO ()
runDb_ action config = do
  pipe <- connect (host (dbHostname config))
  result <- access pipe master (dbName config) action
  close pipe

instance FromJSON DbConfig where
  parseJSON = withObject "dbConf"
    $ \o -> do
      dbHostname <- o .: "host"
      dbName <- o .: "database"
      return DbConfig { .. }

instance FromJSON AppEnv where
  parseJSON = withObject "config"
    $ \o -> do
      logPath <- o .: "logPath"
      configEnvPort <- o .: "port"
      dbConf <- o .: "dbConfig"
      dbHostname <- dbConf .: "host"
      dbName <- dbConf .: "database"
      let dbConfig = DbConfig { .. }
      return (AppEnv dbConfig logPath configEnvPort)
