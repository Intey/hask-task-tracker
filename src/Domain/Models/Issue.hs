{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.Models.Issue where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Swagger (ToSchema)
import           GHC.Generics (Generic)
import           Domain.Models

data Issue = Issue { issueKey :: Key Issue
                   , issueSummary :: String
                   , issueDescription :: Maybe String
                   , issueAssignee :: Maybe (Key User)
                   , issueReporter :: Key User
                   , issueLinkedIssues :: [Key Issue]
                   }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)