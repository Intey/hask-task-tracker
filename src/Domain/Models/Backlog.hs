{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Models.Backlog where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Swagger (ToSchema)
import           Domain.Models (Key)
import           Domain.Models.Project (Project)
import           Domain.Models.Issue
import           Domain.Models.Sprint
import           GHC.Generics (Generic)

data BackLog = BackLog { backlogProject :: Key Project
                       , backlogIssues :: [Issue]
                       , backlogSprints :: [Sprint]
                       }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
