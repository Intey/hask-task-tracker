{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.InputBounds.CreateProject where

import           Data.Aeson    (FromJSON, ToJSON, parseJSON, withObject, (.:),
                                (.:?))
import           Data.Swagger  (ToSchema)
import           Domain.Models (Key, User)
import           GHC.Generics  (Generic)

data CreateProjectSchema = CreateProject
  { owner       :: Key User
  , name        :: String
  , description :: Maybe String
  }
  deriving (Show, Generic, ToSchema, FromJSON)

