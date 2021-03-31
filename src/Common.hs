{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Common where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.Aeson
import           Database.MongoDB

import           Types
import Control.Exception.Base

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

instance FromJSON DbConfig where
  parseJSON = withObject "dbConf" $ \o -> do
    dbHostname <- o .: "host"
    dbName     <- o .: "database"
    return DbConfig{..}


instance FromJSON AppEnv where
  parseJSON = withObject "config" $ \o -> do
    logPath    <- o      .: "logPath"
    dbConf     <- o      .: "dbConfig"
    dbHostname <- dbConf .: "host"
    dbName     <- dbConf .: "database"
    let dbConfig = DbConfig {..}
    return (AppEnv dbConfig logPath)
