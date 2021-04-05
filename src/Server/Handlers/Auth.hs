{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Server.Handlers.Auth where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Servant
import           Servant.Auth        as SA
import           Servant.Auth.Server as SAS

import           Authentica
import           Common              (runDb)
import qualified Domain.Interfaces   as DI
import           Domain.Models
import qualified Storage
import           Types


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


type LoginApi =
  "login"
      :> ReqBody '[JSON] LoginForm
      :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "register"
      :> ReqBody '[JSON] RegisterForm
      :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)


-- type IssuesAPI = "issues" :> Get '[JSON] [Issue]
                     -- :<|> ReqBody '[JSON] Issue :> Post '[JSON] NoContent)
