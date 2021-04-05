{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.Models
where

import           GHC.Generics (Generic)

import Data.Aeson ( FromJSON, ToJSON )
import Servant.Auth.Server.Internal.JWT ( FromJWT, ToJWT )

newtype Key mdl = Key String deriving (Show, Eq, Generic, FromJSON, ToJSON)


data User = User
  { username :: String
  , password :: String
  , userKey  :: Key User
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, FromJWT, ToJWT)


data Issue = Issue
  { issueKey          :: Key Issue 
  , issueSummary      :: String
  , issueDescription  :: Maybe String
  , issueAssignee     :: Maybe (Key User)
  , issueReporter     :: Key User
  , issueLinkedIssues :: [Key Issue]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)


data IssueField = Summary
                | Assignee
                | Reporter
                | Description
                deriving (Show, Generic, FromJSON, ToJSON)


data Project = Project
  { projectKey         :: Key Project
  , projectName        :: String
  , projectDescription :: String
  , projectOwner       :: Key User
  , projectIssues      :: [Issue]
  , projectWorkflow    :: Maybe Workflow
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Workflow = Workflow 
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


data Sprint = Sprint
  { sprintKey  :: Key Sprint
  , sprintName :: String
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)


data BackLog = BackLog
  { backlogProject  :: Key Project
  , backlogIssues   :: [Issue]
  } deriving (Show, Generic, FromJSON, ToJSON)


data BackLogScreen = BackLogScreen 
  { backlogScreenProject    :: String
  , backlogScreenIssues     :: [Issue]
  , backlogScreenSprints    :: [Sprint]
  , backlogScreenIssueView  :: IssueViewConfig
  } deriving (Show, Generic, FromJSON, ToJSON)


newtype BackLogConfig = BackLogConfig
  { issueViewConfig :: IssueViewConfig
  } deriving (Show, Generic, FromJSON, ToJSON)


-- | IssueViewConfig define which fields from issue should be visible
newtype IssueViewConfig = IssueViewConfig [IssueField] 
  deriving (Show, Generic, FromJSON, ToJSON)