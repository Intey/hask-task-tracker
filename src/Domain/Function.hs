{-# LANGUAGE DeriveFunctor #-}

module Domain.Function where

import           Control.Monad.State
import           Data.Char                        (toUpper)
import           Data.Default
import           Domain.Interfaces                as DI
import           Domain.Models

import qualified Domain.InputBounds.CreateIssue   as IB
import qualified Domain.InputBounds.CreateProject as IB

createIssue ::
  (Monad m, IssuesStorage m) =>
  Key User ->
  Key Project ->
  String ->
  Maybe String ->
  m Issue
createIssue r p s d = do
  let inputIssue = IB.CI s d Nothing r []
  key <- saveIssue inputIssue
  return $ Issue key s d Nothing r []

-- | createProject gets owner, name and descritpion, and creates new project
createProject ::
  (ProjectStorage m) =>
  Key User ->
  String ->
  String ->
  m (Key Project)
createProject owner name descr = do
  DI.createProject $
    def
      { projectKey = Key $ map toUpper name,
        projectName = name,
        projectDescription = descr,
        projectOwner = owner
      }

getBacklogScreen ::
  (Monad m, IssuesStorage m, ProjectStorage m, SprintStorage m) =>
  Key Project ->
  m (Maybe BackLogScreen)
getBacklogScreen pk = do
  issues <- loadIssues pk
  pn <- loadProjectName pk
  case pn of
    Just n -> do
      ivc <- loadIssueViewConfig pk -- TODO: typeclass to load backlog config?
      sprints <- loadSprints pk
      case ivc of
        Just i  -> pure . Just $ BackLogScreen n issues sprints i
        Nothing -> pure Nothing
    Nothing -> pure Nothing
