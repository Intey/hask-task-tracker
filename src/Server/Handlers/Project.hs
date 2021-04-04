
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings, TypeOperators, DeriveAnyClass, DeriveGeneric #-}

module Server.Handlers.Project
( projectServer
, ProjectAPI
)
where
import qualified Domain.Function   as DF
import qualified Domain.Interfaces as DI
import Domain.Models
    ( BackLogConfig, BackLog, Project, Issue, Key(Key), BackLogScreen, User)
import           Servant
import           Server.Types -- instances
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe ( fromMaybe )
import Types ( AppM )

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
    { owner :: Key User
    , name :: String
    , description :: Maybe String
} deriving (Show, Generic, FromJSON)


getBacklogScreen :: String -> AppM BackLogScreen
getBacklogScreen k = do
    s <- DF.getBacklogScreen (Key k)
    case s of
        Just screen -> pure screen
        Nothing -> throwError err404


projectDetailsHandler :: String -> AppM Project
projectDetailsHandler = undefined


postProject :: CreateProjectSchema -> AppM (Key Project)
postProject (CreateProject o n d) = DF.createProject o n (fromMaybe "No description" d)


getBacklogConfig :: String -> AppM BackLogConfig
getBacklogConfig = undefined 


postBacklogConfig :: String -> AppM BackLogConfig
postBacklogConfig = undefined 


projectServer :: ServerT ProjectAPI AppM
projectServer projectKey = projectDetailsHandler projectKey 
                      :<|> postProject
                      :<|> getBacklogScreen projectKey 
                      :<|> getBacklogConfig projectKey 
                      :<|> postBacklogConfig projectKey