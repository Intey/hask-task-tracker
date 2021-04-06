{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.InputBounds where

import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:), (.:?))
import Domain.Models
import GHC.Generics (Generic)
import Data.Swagger

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
  deriving (Show, Eq, FromJSON, Generic, ToSchema)


-- | CreateProject owner, projectName, projectDescription
data CreateProjectSchema = CreateProject (Key User) String (Maybe String) 
  deriving (Show, Generic, ToSchema)

{- make direct instance for parse json, 
  because CreateProject fields clashes with CreateIssue, 
  and i'm too lazy for split them by files
-}
instance FromJSON CreateProjectSchema where
    parseJSON = withObject "CreateProjectSchema" $ \v -> CreateProject
        <$> v .: "owner"
        <*> v .: "name"
        <*> v .:? "description" -- optional key
