module Domain.Interfaces where

import           Domain.InputBounds.CreateIssue (CreateIssueSchema)
import           Domain.Models (Key)
import           Domain.Models.Project (Project)
import           Domain.Models.BacklogScreen
import           Domain.Models.Issue
import           Domain.Models.Sprint

class ProjectStorage m where
  loadProject :: Key Project -> m (Maybe Project)
  saveProject :: Project -> m (Either String ())
  createProject :: Project -> m (Key Project)
  loadIssueViewConfig :: Key Project -> m (Maybe IssueViewConfig)
  loadProjectName :: Key Project -> m (Maybe String)

class IssuesStorage m where
  loadIssues :: Key Project -> m [Issue]
  saveIssue :: CreateIssueSchema -> m (Key Issue)

class SprintStorage m where
  loadSprints :: Key Project -> m [Sprint]
