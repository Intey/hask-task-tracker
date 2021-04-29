{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.InputBounds.CreateUser where

import           Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:)
                           , (.:?))
import           Data.Bson.Generic (ToBSON)
import           Data.Swagger (ToSchema)
import           Domain.Models (Key, User)
import           GHC.Generics (Generic)

data CreateUserSchema = CreateUserSchema { username :: String }
  deriving (Show, Generic, ToSchema, FromJSON, ToBSON)
