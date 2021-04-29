{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- {-# LANGUAGE FlexibleInstances #-}
module Domain.Models.Backlog where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Swagger (ToSchema)
import           Domain.Models (Issue, Key, Sprint)
import           Domain.Models.Project (Project)
import           GHC.Generics (Generic)

data BackLog = BackLog { backlogProject :: Key Project
                       , backlogIssues :: [Issue]
                       , backlogSprints :: [Sprint]
                       }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
