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

type IssuesAPI = "issues"
  :> ((ReqBody '[JSON] CreateIssueSchema :> Post '[JSON] (Key Issue)))

postIssue :: CreateIssueSchema -> AppM (Key Issue)
postIssue (CI sumr descr assignee reporter links prj) =
  issueKey <$> DF.createIssue reporter prj sumr descr links

issuesServer :: ServerT IssuesAPI AppM
issuesServer = postIssue