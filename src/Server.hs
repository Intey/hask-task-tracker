{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server
    ( app
    ) where

import           Data.Aeson
import           Network.Wai

import           Servant

import           Common
import           Control.Applicative  (Applicative, liftA2, (<$>), (<*>))
import           Control.Monad.Reader
import           Database.MongoDB
import           Models
import qualified Storage
import           Types                (AppM, AppEnv)

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

nt :: AppEnv -> AppM a -> Handler a
nt s x = liftIO $ runReaderT x s

app :: AppEnv -> Application
app s = serve api $ hoistServer api (nt s) server

-- https://github.com/mauriciofierrom/servant-mongodb/blob/master/src/Common/Types.hs

