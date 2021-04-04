{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Authentica where
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Bson.Generic
import qualified Data.ByteString.Char8 as BS
import           Database.MongoDB      as Mongo
import           GHC.Generics          (Generic)
import           Domain.Models
import           Servant
import           Servant.Auth          as SA
import           Servant.Auth.Server   as SAS
import           Storage

data AuthUser = AUser { id     :: String
                      , groups :: String
} deriving (Show, Generic)

authUserFromUser :: Maybe User -> Maybe AuthUser
authUserFromUser (Just (User username _ _)) = Just $ AUser username ""
authUserFromUser Nothing = Nothing 

instance ToJSON AuthUser
instance FromJSON AuthUser
instance ToJWT AuthUser
instance FromJWT AuthUser

instance FromBSON AuthUser

checkAuth ::  BasicAuthData -> Action IO (AuthResult AuthUser)
checkAuth (BasicAuthData login password) = do
    maybe SAS.Indefinite 
        Authenticated . authUserFromUser 
        <$> loadUser (BS.unpack login) -- (BS.unpack password)


type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthUser)

instance FromBasicAuthData AuthUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData
