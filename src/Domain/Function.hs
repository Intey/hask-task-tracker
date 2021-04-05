{-# LANGUAGE DeriveFunctor #-}
module Domain.Function where
import           Control.Monad.State
import           Data.Char           (toUpper)
import qualified Domain.InputBounds  as IB
import           Domain.Interfaces   as DI
import           Domain.Models

createIssue :: (Monad m, IssuesStorage m) =>
  Key User -> Key Project -> String -> Maybe String -> m Issue
createIssue r p s d = do
    let inputIssue = IB.CI s d Nothing r []
    key <- saveIssue inputIssue
    return $ Issue key s d Nothing r []

{-| createProject gets owner, name and descritpion, and creates new project
-}
createProject :: (ProjectStorage m) =>
  Key User -> String -> String -> m (Key Project)
createProject owner name descr = do
    DI.createProject $ Project
        { projectKey = Key $ map toUpper name ++ "-"
        , projectName = name
        , projectDescription = descr
        , projectOwner = owner
        , projectIssues = []
        , projectWorkflow = Nothing
        }


getBacklogScreen :: (Monad m, IssuesStorage m, ProjectStorage m, SprintStorage m) =>
  Key Project -> m (Maybe BackLogScreen)
getBacklogScreen pk = do
    issues <- loadIssues pk
    pn <- loadProjectName pk
    case pn of
      Just n -> do
        ivc <- loadIssueViewConfig pk -- TODO: typeclass to load backlog config?
        sprints <- loadSprints pk
        pure . Just $ BackLogScreen n issues sprints ivc
      Nothing -> do pure Nothing
