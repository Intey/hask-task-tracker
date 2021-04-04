{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Types where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Servant
import           Servant.Auth        as SA
import           Servant.Auth.Server as SAS

import           Authentica
import           Domain.Models
import qualified Domain.Interfaces as DI
import           Types
import qualified Storage
import           Common (runDb)


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



instance DI.IssuesStorage AppM where
    loadIssues k = runDb $ Storage.projectIssues k
    saveIssue = undefined 

instance DI.ProjectStorage AppM where
    loadProject = runDb . Storage.loadProject 
    saveProject = undefined 
    loadIssueViewConfig = undefined
    loadProjectName = undefined

instance DI.SprintStorage AppM where
    loadSprints = undefined


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
