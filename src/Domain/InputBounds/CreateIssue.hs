{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.InputBounds.CreateIssue where

import           Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:)
                           , (.:?))
import           Data.Swagger (ToSchema)
import           Domain.Models (Issue, Key, User)
import           GHC.Generics (Generic)

data CreateIssueSchema =
  CI { summary :: String
     , description :: Maybe String
     , assignee :: Maybe (Key User)
     , reporter :: Key User
     , linkedIssues :: [Key Issue]
     }
  deriving (Show, Generic, ToSchema, FromJSON)
