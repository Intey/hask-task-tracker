{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.Models.BacklogScreen where

import           Data.Swagger
import           Data.Aeson
import           GHC.Generics (Generic)
import           Domain.Models.Issue
import           Domain.Models.Sprint

data BackLogScreen = BackLogScreen { backlogScreenProject :: String
                                   , backlogScreenIssues :: [Issue]
                                   , backlogScreenSprints :: [Sprint]
                                   , backlogScreenIssueView :: IssueViewConfig
                                   }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- | IssueViewConfig define which fields from issue should be visible
newtype IssueViewConfig = IssueViewConfig [IssueField]
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data IssueField = Summary
                | Assignee
                | Reporter
                | Description
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

newtype BackLogConfig = BackLogConfig { issueViewConfig :: IssueViewConfig }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)