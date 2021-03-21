{-# LANGUAGE DeriveGeneric #-}

module Models
  ( Task(..),
    User(..),
  )
where

import GHC.Generics (Generic)

data User = User {username :: String, fio :: String}
  deriving (Show, Eq, Generic)

data Task = Task {summary :: String, description :: String, assignee :: Maybe User, reporter :: Maybe User}
  deriving (Show, Eq, Generic)
