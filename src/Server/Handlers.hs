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

usersHandler :: AppM [User]
usersHandler = do
--   ctx <- ask
  runDb Storage.allUsers

projectIssuesHandler :: String -> AppM [Issue]
projectIssuesHandler = DI.loadIssues . Key

addUserHandler :: User -> AppM ()
addUserHandler = runDb . Storage.insertUser


getBacklogScreen :: String -> AppM BackLog
getBacklogScreen = DF.getBacklogScreen . Key

projectDetailsHandler :: String -> AppM Project
projectDetailsHandler = undefined 


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


unprotected :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT LoginApi AppM
unprotected cs jwts = loginHandler cs jwts :<|> registerHandler cs jwts


devHandlers :: ServerT DevAPI AppM
devHandlers = (\x -> projectDetailsHandler x :<|> getBacklogScreen x) :<|> addUserHandler :<|> usersHandler
