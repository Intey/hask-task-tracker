{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.Handlers where

import           Authentica
import           Common
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text            (unpack)
import           Models
import           Servant
import           Servant.Auth         as SA
import           Servant.Auth.Server  as SAS
import           Server.Types

import           Debug.Trace
import qualified Storage
import           Types
instance ToJSON Task
instance ToJSON User
instance FromJSON User
instance ToJWT User
instance FromJWT User

usersHandler :: AppM [User]
usersHandler = do
  ctx <- ask
  runDb Storage.allUsers

tasksHander :: AppM [Task]
tasksHander = runDb Storage.allTasks

addUserHandler :: User -> AppM ()
addUserHandler = runDb . Storage.insertUser

loginHandler :: CookieSettings
             -> JWTSettings
             -> LoginForm
             -> AppM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler cookieSettings jwtSettings (LoginForm username pass) = do
    usr <- trace "rungetUser" runDb $ Storage.loadUser (unpack username)
    case usr of
        Just u -> do
            mApplyCookies <- trace "accept login" liftIO $ SAS.acceptLogin cookieSettings jwtSettings u
            case mApplyCookies of
                Nothing           -> trace "notApplyed cooks" throwError err401
                Just applyCookies -> trace "cookies applyed" return $ applyCookies NoContent
        Nothing -> trace "not found " throwError err401
-- loginHandler _ _ _ = throwError err401


protected :: AuthResult AuthUser -> ServerT API AppM
protected (Authenticated usr) = tasksHander :<|> addUserHandler :<|> usersHandler
protected _ = throwAll err403

unprotected :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT LoginApi AppM
unprotected = loginHandler


