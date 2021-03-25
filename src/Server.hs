{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
    ( startApp
    ) where

import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Log.Logger
import Control.Applicative ((<$>), (<*>), liftA2, Applicative)
import Models
import Control.Monad.Reader
import qualified Storage
import Database.MongoDB
import Types ( AppM )
import Common

comp = "LoggingExample.Main"


type TasksAPI = ("tasks" :> Get '[JSON] [Task])
type UsersAPI = ("users" :> Get '[JSON] [User])
type AddUserAPI = ("users" :> ReqBody '[JSON] User :> Post '[JSON] ())

type API = "api" :> (TasksAPI :<|> UsersAPI :<|> AddUserAPI)


instance ToJSON Task
instance ToJSON User
instance FromJSON User

api :: Proxy API
api = Proxy

usersHandler :: AppM [User]
usersHandler = runDb Storage.allUsers

tasksHander :: AppM [Task]
tasksHander = runDb Storage.allTasks

addUserHandler :: User -> AppM ()
addUserHandler = runDb . Storage.insertUser
  
server :: ServerT API AppM
server = tasksHander :<|> usersHandler :<|> addUserHandler

nt :: String -> AppM a -> Handler a
nt s x = liftIO $ runReaderT x s

app :: String -> Application
app s = serve api $ hoistServer api (nt s) server

startApp :: IO ()
startApp = do
  updateGlobalLogger comp (setLevel NOTICE)
  let port = 1234
  noticeM comp $ "start server on port " ++ show port
  run port (app "")
-- https://github.com/mauriciofierrom/servant-mongodb/blob/master/src/Common/Types.hs

