module Domain.Interfaces where
import           Domain.InputBounds (CreateIssue)
import           Domain.Models      (Issue, IssueViewConfig, Key, Project)


class ProjectStorage m where
    loadProject :: Key Project -> m (Maybe Project)
    saveProject :: Project -> m (Key Project)
    loadIssueViewConfig :: Key Project -> m IssueViewConfig


class IssuesStorage m where
    loadIssues :: Key Project -> m [Issue]
    saveIssue :: CreateIssue -> m (Key Issue)
