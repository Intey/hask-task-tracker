{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds       #-}

module Server
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Log.Logger
import Control.Applicative ((<$>), (<*>), liftA2, Applicative)
import Database.MongoDB
import Models
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Monad.Reader
-- import Control.Monad.Trans


comp = "LoggingExample.Main"


type TasksAPI = ("tasks" :> Get '[JSON] [Task])
type UsersAPI = ("users" :> Get '[JSON] [User])
type AddUserAPI = ("users" :> ReqBody '[JSON] User :> Post '[JSON] ())

type API = "api" :> (TasksAPI :<|> UsersAPI :<|> AddUserAPI)

type AppM = ReaderT State Handler

instance ToJSON Task
instance ToJSON User
instance FromJSON User

data State = State {
  users :: TVar [User],
  tasks :: TVar [Task]
}

api :: Proxy API
api = Proxy

usersHandler :: AppM [User]
usersHandler = do
  State{users=p} <- ask
  liftIO $ readTVarIO p

tasksHander :: AppM [Task]
tasksHander = do
  State{tasks=p} <- ask 
  liftIO $ readTVarIO p

addUserHandler :: User -> AppM ()
addUserHandler usr = do
  State{users=p} <- ask
  liftIO . atomically $ modifyTVar p (usr:)
  
server :: ServerT API AppM
server = tasksHander :<|> usersHandler :<|> addUserHandler

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

startApp :: IO ()
startApp = do
  updateGlobalLogger comp (setLevel NOTICE)
  noticeM comp $ "start server on port " ++ show port
  initUsers <- newTVarIO []
  initTasks <- newTVarIO []
  run port $ app $ State initUsers initTasks
  where port = 1234


