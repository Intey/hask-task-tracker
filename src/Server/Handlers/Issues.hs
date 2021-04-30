{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.Issues where

import           Servant
import           Domain.Models
import           Domain.InputBounds.CreateIssue
import           Types
import qualified Domain.Function as DF
import           Domain.Models.Issue
import           Server.Types
import Domain.Models.Project

type IssuesAPI = "issues"
  :> (ReqBody '[JSON] CreateIssueSchema :> Post '[JSON] (Key Issue))

postIssue :: Key Project -> CreateIssueSchema -> AppM (Key Issue)
postIssue prj (CI sumr descr assignee reporter link) =
  issueKey <$> DF.createIssue reporter prj sumr descr links

issuesServer :: Key Project -> ServerT IssuesAPI AppM
issuesServer = postIssue