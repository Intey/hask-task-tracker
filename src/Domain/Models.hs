{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.Models where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import GHC.Generics (Generic)
import Servant.Auth.Server.Internal.JWT (FromJWT, ToJWT)
import Data.Swagger

newtype Key mdl = Key String deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data User = User
  { username :: String,
    password :: String,
    userKey :: Key User
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, FromJWT, ToJWT, ToSchema)

data Issue = Issue
  { issueKey :: Key Issue,
    issueSummary :: String,
    issueDescription :: Maybe String,
    issueAssignee :: Maybe (Key User),
    issueReporter :: Key User,
    issueLinkedIssues :: [Key Issue]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data IssueField
  = Summary
  | Assignee
  | Reporter
  | Description
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data Project = Project
  { projectKey :: Key Project,
    projectName :: String,
    projectDescription :: String,
    projectOwner :: Key User,
    projectIssues :: [Issue]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data Workflow = Workflow
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data Sprint = Sprint
  { sprintKey :: Key Sprint,
    sprintName :: String
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data BackLog = BackLog
  { backlogProject :: Key Project,
    backlogIssues :: [Issue]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data BackLogScreen = BackLogScreen
  { backlogScreenProject :: String,
    backlogScreenIssues :: [Issue],
    backlogScreenSprints :: [Sprint],
    backlogScreenIssueView :: IssueViewConfig
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype BackLogConfig = BackLogConfig
  { issueViewConfig :: IssueViewConfig
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | IssueViewConfig define which fields from issue should be visible
newtype IssueViewConfig = IssueViewConfig [IssueField]
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

instance Default Project where
  def = Project (Key "") "" "" (Key "") []
