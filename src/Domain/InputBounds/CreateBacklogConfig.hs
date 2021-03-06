{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.InputBounds.CreateBacklogConfig where

import           Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:)
                           , (.:?))
import           Data.Swagger
import           GHC.Generics (Generic)
import           Domain.Models
import           Domain.Models.BacklogScreen
import           Domain.Models.Workflow

data CreateBacklogConfig =
  CBC { configIssueViewConfig :: IssueViewConfig, configWorkflow :: Workflow }
  deriving (Show, Eq, FromJSON, Generic, ToSchema)



