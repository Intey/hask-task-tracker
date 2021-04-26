{-# LANGUAGE FlexibleInstances #-}

module Server.Types where

import           Common              (runDb)
import qualified Domain.Interfaces   as DI
import qualified Storage
import           Types
import Domain.Models

instance DI.IssuesStorage AppM where
    loadIssues k = runDb $ Storage.projectIssues k
    saveIssue = undefined

instance DI.ProjectStorage AppM where
    loadProject = runDb . Storage.loadProject
    saveProject = undefined
    loadIssueViewConfig = runDb . Storage.loadIssueViewConfig
    loadProjectName = runDb . Storage.loadProjectName
    createProject = runDb . Storage.createProject

instance DI.SprintStorage AppM where
    loadSprints = undefined
