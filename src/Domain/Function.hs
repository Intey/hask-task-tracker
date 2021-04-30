{-# LANGUAGE DeriveFunctor #-}

module Domain.Function where

import           Control.Monad.State
import           Data.Char (toUpper)
import           Data.Default
import qualified Domain.InputBounds.CreateIssue as IB
import qualified Domain.InputBounds.CreateProject as IB
import           Domain.Interfaces as DI
import           Domain.Models
import qualified Domain.Models.Project as P
import           Domain.Models.Issue
import           Domain.Models.BacklogScreen

createIssue :: (Monad m, IssuesStorage m)
            => Key User
            -> Key P.Project
            -> String
            -> Maybe String
            -> [Key Issue]
            -> m (Key Issue)
createIssue r p s d links = do
  let inputIssue = Issue (Key "") s d Nothing r links
  DI.saveIssue inputIssue

-- | createProject gets owner, name and descritpion, and creates new project
createProject
  :: (ProjectStorage m) => Key User -> String -> String -> m (Key P.Project)
createProject owner name descr = do
  DI.createProject
    $ def { P.key = Key $ map toUpper name
          , P.name = name
          , P.description = descr
          , P.owner = owner
          }

getBacklogScreen
  :: (Monad m, IssuesStorage m, ProjectStorage m, SprintStorage m)
  => Key P.Project
  -> m (Maybe BackLogScreen)
getBacklogScreen pk = do
  issues <- loadIssues pk
  pn <- loadProjectName pk
  case pn of
    Just n  -> do
      ivc <- loadIssueViewConfig pk -- TODO: typeclass to load backlog config?
      sprints <- loadSprints pk
      case ivc of
        Just i  -> pure . Just $ BackLogScreen n issues sprints i
        Nothing -> pure Nothing
    Nothing -> pure Nothing
