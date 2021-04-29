{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.Models.Sprint where

import           Data.Swagger (ToSchema)
import           Data.Aeson
import           GHC.Generics (Generic)
import           Domain.Models.Issue

data Sprint =
  Sprint { sprintId :: String, sprintGoal :: String, sprintIssues :: [Issue] }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)