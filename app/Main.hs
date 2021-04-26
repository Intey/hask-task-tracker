{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Domain.Models
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server as SAS
import Servant.Swagger
import Server.Api
import Server.App (mkDevApp)
import System.Log.Logger

  ( Priority (NOTICE),
    noticeM,
    setLevel,
    updateGlobalLogger,
  )
import qualified Types as T
import Common ( runDb_ )
import qualified Storage 


comp = "LoggingExample.Main"

-- TODO: configuration from file or venv

main' :: IO ()
main' = do
  myKey <- generateKey
  configFile <- BL.readFile "config.json"
  let appEnv = (decode configFile :: Maybe T.AppEnv)
      jwtCfg = defaultJWTSettings myKey
      cookieCfg =
        if True -- get from environment or config
          then defaultCookieSettings {cookieIsSecure = SAS.NotSecure}
          else defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext

  updateGlobalLogger comp (setLevel NOTICE)
  let port = 3000
  noticeM comp $ "start server on port " ++ show port
  (putStrLn . toString) configFile
  case appEnv of
    Nothing -> error "Configuration could not be loaded"
    Just env -> do
      runDb_ Storage.initDB (T.dbConfig env)
      run port (mkDevApp env) -- (mkApp cfg cookieCfg jwtCfg env)

getSwagger :: String
getSwagger =  BLC.unpack $ encode $ swaggerAPI

main = do
  let sw = getSwagger
  writeFile "swagger.json" sw
  putStrLn sw
  main' 
