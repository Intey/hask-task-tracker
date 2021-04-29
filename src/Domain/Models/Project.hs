{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.Models.Project where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Default (Default(..))
import           Data.Swagger (ToSchema)
import           Domain.Models
import           GHC.Generics (Generic)

data Project = Project { key :: Key Project
                       , name :: String
                       , description :: String
                       , owner :: Key User
                       , issues :: [Issue]
                       }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

instance Default Project where
  def = Project (Key "") "" "" (Key "") []
