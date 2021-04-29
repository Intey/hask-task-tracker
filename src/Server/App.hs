{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.App
    (   -- mkApp
      mkDevApp) where

import           Network.Wai
import           Servant
import           Control.Applicative (Applicative, liftA2, (<$>), (<*>))
import           Control.Monad (ap)
import           Control.Monad.Reader
import           Servant.Auth as SA
import qualified Storage
import           Types (AppEnv, AppM)
import           Server.Api (DevAPI, FullAPI, devHandlers)

-- server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT (FullAPI auths) AppM
-- server cs jwts = protected :<|> unprotected cs jwts
devServer = devHandlers

-- handlerWithDB :: AppEnv -> AppM a -> Handler a
-- handlerWithDB env action = runReaderT action env
apiAndLogin :: Proxy (FullAPI '[JWT])
apiAndLogin = Proxy

-- mkApp
-- app :: AppEnv -> Application
-- app s = serve apiAndLogin $ hoistServer apiAndLogin (handlerWithDB s) server
-- mkApp :: Context '[SAS.CookieSettings, SAS.JWTSettings] -> SAS.CookieSettings -> SAS.JWTSettings -> AppEnv -> Application
-- mkApp ctx cooks jwts env =
--   serveWithContext apiAndLogin ctx $
--     hoistServerWithContext apiAndLogin (Proxy :: Proxy '[SAS.CookieSettings, SAS.JWTSettings])
--     (`runReaderT` env) (server cooks jwts)
mkDevApp :: AppEnv -> Application
mkDevApp env = serve (Proxy :: Proxy DevAPI)
  $ hoistServer (Proxy :: Proxy DevAPI) (`runReaderT` env) devServer
-- https://github.com/mauriciofierrom/servant-mongodb/blob/master/src/Common/Types.hs
