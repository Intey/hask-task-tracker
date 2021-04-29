{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Models where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Default
import           Data.Swagger
import           GHC.Generics (Generic)
import           Servant.Auth.Server.Internal.JWT (FromJWT, ToJWT)

newtype Key mdl = Key String
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data User = User { username :: Key User, userId :: String }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, FromJWT, ToJWT, ToSchema)