{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server.Types where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Servant
import           Servant.Auth        as SA
import           Servant.Auth.Server as SAS

import           Authentica
import           Models

type LogMessage = String

type LoginApi =
  "login"
      :> ReqBody '[JSON] LoginForm
      :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    
data LoginForm = LoginForm {
  username   :: Text
  , password :: Text
} deriving (Eq, Show, Generic)

data RegisterForm = RegisterForm {
  regFormUsername :: Text
  , regFormPassword :: Text
} deriving (Show, Generic)

instance ToJSON LoginForm
instance FromJSON LoginForm
instance ToJSON RegisterForm
instance FromJSON RegisterForm


type TasksAPI = ("tasks" :> Get '[JSON] [Task])
type UsersAPI = ("users" :> ReqBody '[JSON] User :> Post '[JSON] () :<|> Get '[JSON] [User])

type API = "api" :> TasksAPI :<|> UsersAPI
type FullAPI auths = (Auth auths AuthUser :> API) :<|> LoginApi
