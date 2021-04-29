{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Domain.Models.Backlog where
import Domain.Models
import Domain.Models.Project

data BackLog = BackLog
  { backlogProject :: Key Project
  , backlogIssues  :: [Issue]
  , backlogSprints :: [Sprint]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
