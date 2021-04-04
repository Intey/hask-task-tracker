{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Monad.Reader (ReaderT)
import           Data.Text            (Text)
import           Servant
type AppM = ReaderT AppEnv Handler

data DbConfig =
  DbConfig { dbHostname :: String
           , dbName     :: Text
           }
           deriving (Show)

data AppEnv =
  AppEnv { dbConfig :: DbConfig
         , logPath  :: String }