{-# LANGUAGE TypeOperators, DataKinds #-}

module Server.Handlers.User where
import Servant
import Domain.Models (User)
import Storage
import Types 
import Common 

type UsersAPI = "users" :> ( ReqBody '[JSON] User :> Post '[JSON] ()
                    :<|> Get '[JSON] [User]
                    )

usersHandler :: AppM [User]
usersHandler = do
--   ctx <- ask
  runDb Storage.allUsers
  
addUserHandler :: User -> AppM ()
addUserHandler = runDb . Storage.insertUser


usersServer = addUserHandler :<|> usersHandler