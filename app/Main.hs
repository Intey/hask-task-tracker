{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.Wai.Handler.Warp (run)
import           Server                   (app)
import           System.Log.Logger        (Priority (NOTICE), noticeM, setLevel,
                                           updateGlobalLogger)
import qualified Data.ByteString.Lazy as BL
import Types
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (toString)


comp = "LoggingExample.Main"


main :: IO ()
main = do
  configFile <- BL.readFile "config.json"
  updateGlobalLogger comp (setLevel NOTICE)
  let port = 1234
  noticeM comp $ "start server on port " ++ show port
  (putStrLn . toString) configFile
  let appEnv = (decode configFile :: Maybe AppEnv)
  case appEnv of
    Nothing     -> error "Configuration could not be loaded"
    Just env -> run port (app env)
