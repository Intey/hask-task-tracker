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
import           Domain.Models

type LogMessage = String


data LoginForm = LoginForm {
  username   :: Text
  , password :: Text
} deriving (Eq, Show, Generic)


data RegisterForm = RegisterForm {
  regFormUsername   :: Text
  , regFormPassword :: Text
} deriving (Show, Generic)


instance ToJSON LoginForm
instance FromJSON LoginForm
instance ToJSON RegisterForm
instance FromJSON RegisterForm

instance ToJSON (Key User)
instance FromJSON (Key User)
instance ToJSON (Key Project)
instance FromJSON (Key Project)
instance ToJSON (Key Issue)
instance FromJSON (Key Issue)
instance ToJSON Issue
instance FromJSON Issue
instance ToJSON User
instance FromJSON User
instance ToJWT User
instance FromJWT User
instance ToJSON Project 
instance FromJSON Project 
instance ToJSON IssueField 
instance FromJSON IssueField
instance ToJSON IssueViewConfig 
instance FromJSON IssueViewConfig 
instance ToJSON BackLog 
instance FromJSON BackLog 


instance DI.IssuesStorage AppM where
    loadIssues k = runDb $ Storage.projectIssues k
    saveIssue = undefined 


instance DI.ProjectStorage AppM where
    loadProject = runDb . Storage.loadProject 
    saveProject = undefined 
    loadIssueViewConfig = undefined


type ProjectAPI =
    "projects" :> Capture "projectKey" String 
    :> ( Get '[JSON] Project
    :<|> "board" 
      :> ( Get '[JSON] BackLog 
      :<|> "config" :> (Get '[JSON] BackLogConfig :<|> Post '[JSON] BackLogConfig )
      )
    )


type LoginApi =
  "login"
      :> ReqBody '[JSON] LoginForm
      :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "register"
      :> ReqBody '[JSON] RegisterForm
      :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)


-- type IssuesAPI = "issues" :> Get '[JSON] [Issue]
                     -- :<|> ReqBody '[JSON] Issue :> Post '[JSON] NoContent)
type UsersAPI = "users" :> ( ReqBody '[JSON] User :> Post '[JSON] ()
                        :<|> Get '[JSON] [User]
                        )


type API = "api" :> (ProjectAPI  :<|> UsersAPI)
type FullAPI auths = (Auth auths AuthUser :> API) :<|> LoginApi
type DevAPI = API
