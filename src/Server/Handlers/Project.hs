
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handlers.Project where
import qualified Domain.Function   as DF
import qualified Domain.Interfaces as DI
import qualified Domain.Models as DM
import           Servant
import           Server.Types


projectIssuesHandler :: String -> AppM [DM.Issue]
projectIssuesHandler = DI.loadIssues . DM.Key

getBacklogScreen :: String -> AppM DM.BackLog
getBacklogScreen = DF.getBacklogScreen . DM.Key

projectDetailsHandler :: String -> AppM DM.Project
projectDetailsHandler = undefined
