
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Handlers.Project
( projectServer
, ProjectAPI
)
where
import           Data.Aeson                 (FromJSON)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Maybe                 (fromMaybe)
import qualified Domain.Function            as DF
import qualified Domain.Interfaces          as DI
import           Domain.Models              (BackLog, BackLogConfig,
                                             BackLogScreen, Issue, Key (Key),
                                             Project, User)
import           GHC.Generics               (Generic)
import           Servant
import           Server.Types
import           Types                      (AppM)
import qualified Storage
import Common 

type ProjectAPI =
    "projects"
    :> Capture "projectKey" String
    :> ( Get '[JSON] Project
    :<|> (ReqBody '[JSON] CreateProjectSchema :> Post '[JSON] (Key Project))
    :<|> "board"
        :> ( Get '[JSON] BackLogScreen
        :<|> "config"
          :> ( Get '[JSON] BackLogConfig
          :<|> Post '[JSON] BackLogConfig
          )
        )
    )

data CreateProjectSchema = CreateProject
    { owner       :: Key User
    , name        :: String
    , description :: Maybe String
} deriving (Show, Generic, FromJSON)


getBacklogScreen :: String -> AppM BackLogScreen
getBacklogScreen k = do
    s <- DF.getBacklogScreen (Key k)
    case s of
        Just screen -> pure screen
        Nothing     -> throwError err404


projectDetailsHandler :: String -> AppM Project
projectDetailsHandler k = do
    prj <- runDb $ Storage.loadProject (Key k)
    case prj of
        Just p -> pure p
        Nothing  -> throwError err404


postProject :: CreateProjectSchema -> AppM (Key Project)
postProject (CreateProject o n d) = DF.createProject o n (fromMaybe "No description" d)


postBacklogConfig :: String -> AppM BackLogConfig
postBacklogConfig = undefined


getBacklogConfig :: String -> AppM BackLogConfig
getBacklogConfig = undefined


projectServer :: ServerT ProjectAPI AppM
projectServer projectKey = projectDetailsHandler projectKey
                      :<|> postProject
                      :<|> getBacklogScreen projectKey
                      :<|> getBacklogConfig projectKey
                      :<|> postBacklogConfig projectKey
