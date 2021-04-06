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
import Types

comp = "LoggingExample.Main"

main' :: IO ()
main' = do
  configFile <- BL.readFile "config.json"
  updateGlobalLogger comp (setLevel NOTICE)
  let port = 3000
  noticeM comp $ "start server on port " ++ show port
  (putStrLn . toString) configFile
  myKey <- generateKey
  let appEnv = (decode configFile :: Maybe AppEnv)
      jwtCfg = defaultJWTSettings myKey
      cookieCfg =
        if True -- get from environment or config
          then defaultCookieSettings {cookieIsSecure = SAS.NotSecure}
          else defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext
  case appEnv of
    Nothing -> error "Configuration could not be loaded"
    Just env -> run port (mkDevApp env) -- (mkApp cfg cookieCfg jwtCfg env)

getSwagger :: IO ()
getSwagger = BLC.putStrLn $ encode $ toSwagger (Proxy :: Proxy DevAPI)

main = do
  getSwagger
  main' 
