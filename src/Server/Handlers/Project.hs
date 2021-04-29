
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.Project (projectServer, ProjectAPI) where

import           Common
import           Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char (toUpper)
import           Data.Maybe (fromMaybe)
import           Data.Swagger
import qualified Domain.Function as DF
import           Domain.InputBounds.CreateBacklogConfig
import           Domain.InputBounds.CreateProject
import qualified Domain.Interfaces as DI
import           Domain.Models (Key(Key), User)
import           Domain.Models.Project (Project)
import           GHC.Generics (Generic)
import           Servant
import           Server.Types
import qualified Storage
import           Types (AppM)
import           Domain.Models.BacklogScreen

type ProjectKey = String

type ProjectAPI = "projects"
  :> ((ReqBody '[JSON] CreateProjectSchema :> Post '[JSON] (Key Project))
      :<|> (Capture "projectKey" String
            :> (Get '[JSON] Project :<|> "board"
                :> (Get '[JSON] BackLogScreen :<|> "config"
                    :> (Get '[JSON] BackLogConfig
                        :<|> (ReqBody '[JSON] CreateBacklogConfig
                              :> Post '[JSON] ()))))))

postProject :: CreateProjectSchema -> AppM (Key Project)
postProject (CreateProject o n d) =
  DF.createProject o n (fromMaybe "No description" d)

getBacklogScreen :: ProjectKey -> AppM BackLogScreen
getBacklogScreen k = do
  s <- DF.getBacklogScreen (Key k)
  case s of
    Just screen -> pure screen
    Nothing     -> throwError err404

projectDetailsHandler :: ProjectKey -> AppM Project
projectDetailsHandler k = do
  prj <- runDb $ Storage.loadProject (Key (map toUpper k))
  case prj of
    Just p  -> pure p
    Nothing -> throwError err404

postBacklogConfig :: ProjectKey -> CreateBacklogConfig -> AppM ()
postBacklogConfig k (CBC ivc w) = do
  done <- runDb $ Storage.saveBacklogConfig (Key k) ivc w
  if done
    then pure ()
    else throwError err500 { errBody = "Storage update error?" }

getBacklogConfig :: ProjectKey -> AppM BackLogConfig
getBacklogConfig = undefined

projectServer :: ServerT ProjectAPI AppM
projectServer = postProject
  :<|> \projectKey -> projectDetailsHandler projectKey
  :<|> getBacklogScreen projectKey
  :<|> getBacklogConfig projectKey
  :<|> postBacklogConfig projectKey
