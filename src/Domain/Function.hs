{-# LANGUAGE DeriveFunctor #-}
module Domain.Function where
import           Control.Monad.State
import           Data.Char           (toUpper)
import qualified Domain.InputBounds  as IB
import           Domain.Interfaces
import           Domain.Models

createIssue :: (Monad m, IssuesStorage m) => Key User -> Key Project -> String -> Maybe String -> m Issue
createIssue r p s d = do
    let inputIssue = IB.CI s d Nothing r []
    key <- saveIssue inputIssue
    return $ Issue key s d Nothing r []


createProject :: (ProjectStorage m, MonadState a m, Show a) => Key User -> String -> String -> m (Key Project)
createProject owner name descr = do
    k <- get
    saveProject $ Project
        { projectKey = Key $ map toUpper name ++ show k
        , projectName = name
        , projectDescription = descr
        , projectOwner = owner
        , projectIssues = []
        }


getBacklogScreen :: (Monad m, IssuesStorage m, ProjectStorage m) => Key Project -> m BackLog
getBacklogScreen pk = do
    issues <- loadIssues pk
    -- ivc <- loadIssueViewConfig pk -- TODO: typeclass to load backlog config?
    pure $ BackLog pk issues
