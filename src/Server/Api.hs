{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module Server.Api where
import Servant
import Server.Types
import Authentica
import           Servant.Auth         as SA
import           Servant.Auth.Server  as SAS
import Types
import Server.Handlers
import Server.Handlers.Project
import Server.Handlers.User
import Server.Handlers.Auth

type API = "api" :> (ProjectAPI  :<|> UsersAPI)
type FullAPI auths = (Auth auths AuthUser :> API) :<|> LoginApi
type DevAPI = API


unprotected :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT LoginApi AppM
unprotected cs jwts = loginHandler cs jwts :<|> registerHandler cs jwts


devHandlers :: ServerT DevAPI AppM
devHandlers = projectServer :<|> usersServer
