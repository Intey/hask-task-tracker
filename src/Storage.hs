{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Storage
where

import           Control.Monad       (liftM)
import           Control.Monad.Trans (liftIO)
import           Data.Bson.Generic
import           Database.MongoDB    (Action, Collection, Document,
                                      Select (select), find, findOne, insert_,
                                      rest, (=:))
import qualified Domain.Interfaces   as DI
import           Domain.Models       (Issue, Key(Key), Project (Project), User)

projectsCollection :: Collection
projectsCollection = "projects"

usersCollection :: Collection
usersCollection = "users"

loginCollection :: Collection
loginCollection = "logins"


instance ToBSON (Key User)
instance FromBSON (Key User)
instance ToBSON User
instance FromBSON User

instance ToBSON (Key Issue)
instance FromBSON (Key Issue)
instance ToBSON Issue
instance FromBSON Issue

instance ToBSON (Key Project)
instance FromBSON (Key Project)
instance ToBSON Project
instance FromBSON Project

{-| saveProject updates exist project -}
saveProject :: Project -> Action IO ()
saveProject = insert_ projectsCollection . toBSON

createProject :: Project -> Action IO (Key Project)
createProject = undefined 


loadProject :: Key Project -> Action IO (Maybe Project)
loadProject (Key k) = do
    doc <- findOne (select ["key" =: k] projectsCollection)
    return (doc >>= fromBSON)


projectIssues :: Key Project  -> Action IO [Issue]
projectIssues (Key k) = do
    proj <- findOne (select ["key" =: k] projectsCollection)
    case parseProjectIssues proj of
        Just is -> pure is
        Nothing -> pure []


toIssue :: Action IO [Document] -> Action IO [Issue]
toIssue = fmap (filterNothing . map fromBSON)


toUser :: Action IO [Document] -> Action IO [User]
toUser = fmap (filterNothing . map fromBSON)


allUsers :: Action IO [User]
allUsers = toUser $ rest =<< find (select [] usersCollection)


{- | Saves new user
TODO: Should accepts Input-Bound Struct "NewUser" and return User key
-}
insertUser :: User -> Action IO ()
insertUser = insert_ usersCollection . toBSON


loadUser :: String -> Action IO (Maybe User)
loadUser login = do
    res <- findOne (select ["username" =: login] usersCollection)
    pure $ res >>= fromBSON


parseProjectIssues :: Maybe Document -> Maybe [Issue]
parseProjectIssues doc = do
    d <- doc
    (Project _ _ _ _ issues) <- fromBSON d
    return issues


filterNothing :: [Maybe a] -> [a]
filterNothing tsks = [x | Just x <- tsks]
