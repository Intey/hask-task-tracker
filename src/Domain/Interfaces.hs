module Domain.Interfaces where

import Domain.InputBounds (CreateIssue)
import Domain.Models (Issue, IssueViewConfig, Key, Project, Sprint)

class ProjectStorage m where
  loadProject :: Key Project -> m (Maybe Project)
  saveProject :: Project -> m (Either String ())
  createProject :: Project -> m (Key Project)
  loadIssueViewConfig :: Key Project -> m IssueViewConfig
  loadProjectName :: Key Project -> m (Maybe String)

class IssuesStorage m where
  loadIssues :: Key Project -> m [Issue]
  saveIssue :: CreateIssue -> m (Key Issue)

class SprintStorage m where
  loadSprints :: Key Project -> m [Sprint]
