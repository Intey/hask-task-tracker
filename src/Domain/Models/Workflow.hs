{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.Models.Workflow where

import           Data.Swagger
import           Data.Aeson
import           GHC.Generics (Generic)

data Workflow = Workflow
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)
