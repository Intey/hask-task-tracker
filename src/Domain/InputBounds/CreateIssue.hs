{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.InputBounds.CreateIssue where

import           Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:)
                           , (.:?))
import           Data.Swagger (ToSchema)
import           Domain.Models (Key, User)
import           GHC.Generics (Generic)
import           Domain.Models.Issue
import           Domain.Models.Project

data CreateIssueSchema =
  CI { summary :: String
     , description :: Maybe String
     , assignee :: Maybe (Key User)
     , reporter :: Key User
     , linkedIssues :: [Key Issue]
     , project :: Key Project
     }
  deriving (Show, Generic, ToSchema, FromJSON)
