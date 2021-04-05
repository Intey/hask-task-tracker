{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.Handlers where

import           Authentica
import           Common
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text            (unpack)
import           Domain.Models
import           Servant
import           Servant.Auth         as SA
import           Servant.Auth.Server  as SAS
import           Server.Types

import           Debug.Trace
import qualified Domain.Function      as DF
import qualified Domain.Interfaces    as DI
import qualified Storage
import           Types
import           Server.Handlers.Auth


loginHandler :: CookieSettings
             -> JWTSettings
             -> LoginForm
             -> AppM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler cookieSettings jwtSettings (LoginForm username pass) = do
    usr <- trace "run get User" runDb $ Storage.loadUser (unpack username)
    case usr of
        Just u -> do
            mApplyCookies <- trace "accept login" liftIO $ SAS.acceptLogin cookieSettings jwtSettings u
            case mApplyCookies of
                Nothing           -> trace "notApplyed cooks" throwError err401
                Just applyCookies -> trace "cookies applyed" return $ applyCookies NoContent
        Nothing -> trace "not found " throwError err401
-- loginHandler _ _ _ = throwError err401


registerHandler :: CookieSettings
             -> JWTSettings
             -> RegisterForm
             -> AppM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
registerHandler cookieSettings jwtSettings (RegisterForm username pass) = do
    let user = User (unpack username) (unpack pass) (Key "TODO-KEY")
    trace "register user " runDb $ Storage.insertUser user
    mApplyCookies <- trace "accept login" liftIO $ SAS.acceptLogin cookieSettings jwtSettings user
    case mApplyCookies of
        Nothing           -> trace "notApplyed cooks" throwError err401
        Just applyCookies -> trace "cookies applyed" return $ applyCookies NoContent


-- protected :: AuthResult AuthUser -> ServerT API AppM
-- protected (Authenticated usr) = tasksHander :<|> addUserHandler :<|> usersHandler
-- protected _ = throwAll err403

