{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Api where

import           Authentica
import           Servant
import           Servant.Auth as SA
import           Servant.Auth.Server as SAS
import           Server.Handlers
import           Server.Handlers.Auth
import           Server.Handlers.Project
import           Server.Handlers.User
import           Server.Types
import           Types
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Servant.Swagger
import           Data.Swagger
import           Control.Lens

type SwaggerAPI = ("swagger" :> Get '[JSON] Swagger)

type API = "api" :> (ProjectAPI :<|> UsersAPI)

type FullAPI auths = (Auth auths AuthUser :> API) :<|> LoginApi

type DevAPI = API :<|> SwaggerAPI

unprotected :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT LoginApi AppM
unprotected cs jwts = loginHandler cs jwts :<|> registerHandler cs jwts

swaggerAPI :: Swagger
swaggerAPI = toSwagger (Proxy :: Proxy API) -- (projectServer :<|> usersServer)
  & info . title .~ "Task Tracker API"
  & info . version .~ "1.0"
  & info . description ?~ "Some bitch"
  & info . license ?~ ("MIT" & url ?~ URL "http://mit.com")

devHandlers :: ServerT DevAPI AppM
devHandlers = (projectServer :<|> usersServer) :<|> return swaggerAPI
