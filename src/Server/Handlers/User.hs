{-# LANGUAGE TypeOperators, DataKinds #-}

module Server.Handlers.User where

import           Servant
import           Domain.Models (User, Key)
import           Storage
import           Types
import           Common
import           Domain.InputBounds.CreateUser

type UsersAPI = "users" :> (ReqBody '[JSON] CreateUserSchema
                            :> Post '[JSON] (Key User) :<|> Get '[JSON] [User])

usersHandler :: AppM [User]
usersHandler = do
  runDb Storage.allUsers

addUserHandler :: CreateUserSchema -> AppM (Key User)
-- TODO: return user key
addUserHandler u = runDb $ Storage.insertUser u

usersServer = addUserHandler :<|> usersHandler