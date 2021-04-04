{-# LANGUAGE DeriveGeneric #-}

module Domain.Models
where

import           GHC.Generics (Generic)

newtype Key mdl = Key String deriving (Show, Eq, Generic)

data User = User
  { username :: String
  , password :: String
  , userKey  :: Key User
  } deriving (Show, Eq, Generic)


data Issue = Issue
  { issueKey          :: Key Issue 
  , issueSummary      :: String
  , issueDescription  :: Maybe String
  , issueAssignee     :: Maybe (Key User)
  , issueReporter     :: Key User
  , issueLinkedIssues :: [Key Issue]
  } deriving (Show, Eq, Generic)


data IssueField = Summary
                | Assignee
                | Reporter
                | Description
                deriving (Show, Generic)

data Project = Project
  { projectKey         :: Key Project
  , projectName        :: String
  , projectDescription :: String
  , projectOwner       :: Key User
  , projectIssues      :: [Issue]
  } deriving (Show, Eq, Generic)

data Sprint = Sprint
  { sprintKey  :: Key Sprint
  , sprintName :: String
  } deriving (Show, Eq, Generic)

data BackLog = BackLog
  { backlogProject  :: Key Project
  , backlogIssues   :: [Issue]
  } deriving (Show, Generic)

newtype BackLogConfig = BackLogConfig
  { issueViewConfig :: IssueViewConfig
  }
-- | IssueViewConfig define which fields from issue should be visible
newtype IssueViewConfig = IssueViewConfig [IssueField] 
  deriving (Show, Generic)


