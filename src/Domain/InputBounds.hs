{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.InputBounds where

import Data.Aeson (FromJSON, ToJSON)
import Domain.Models
import GHC.Generics (Generic)

data CreateIssue = CI
  { summary :: String,
    description :: Maybe String,
    assignee :: Maybe (Key User),
    reporter :: Key User,
    linkedIssues :: [Key Issue]
  }

data NewUser = NU {username :: String, password :: String}

data CreateBacklogConfig = CBC
  { configIssueViewConfig :: IssueViewConfig,
    configWorkflow :: Workflow
  }
  deriving (Show, Eq, FromJSON, Generic)
