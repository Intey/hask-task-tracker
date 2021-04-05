{-# LANGUAGE FlexibleInstances #-}

module Server.Types where

import           Common              (runDb)
import qualified Domain.Interfaces   as DI
import qualified Storage
import           Types

instance DI.IssuesStorage AppM where
    loadIssues k = runDb $ Storage.projectIssues k
    saveIssue = undefined

instance DI.ProjectStorage AppM where
    loadProject = runDb . Storage.loadProject
    saveProject = undefined
    loadIssueViewConfig = undefined
    loadProjectName = runDb . Storage.loadBacklog
    createProject = runDb . undefined 

instance DI.SprintStorage AppM where
    loadSprints = undefined
