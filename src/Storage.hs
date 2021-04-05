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
                                      rest, (=:), insert, cast, typed, Query (project), look, findAndModify)
import qualified Domain.Interfaces   as DI
import           Domain.Models       (Issue, Key(Key), Project (Project), User, Workflow)
import qualified Domain.Models as DM
import Data.Functor ( (<&>) )
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
instance FromBSON DM.IssueField
instance ToBSON DM.IssueField
instance FromBSON DM.IssueViewConfig
instance ToBSON DM.IssueViewConfig
instance ToBSON Project
instance FromBSON Project
instance ToBSON Workflow
instance FromBSON Workflow


{-| saveProject updates exist project -}
saveProject :: Project -> Action IO ()
saveProject = insert_ projectsCollection . toBSON


createProject :: Project -> Action IO (Key Project)
createProject p = typed <$> insert projectsCollection (toBSON p)


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

loadProjectName :: Key Project -> Action IO (Maybe String)
loadProjectName (Key k) = do
  proj <- findOne (select ["key" =: k] projectsCollection) {project = ["name" =: 1, "_id" =: 0]}
  return $ proj >>= look "name" >>= cast

saveBacklogConfig :: Key Project -> DM.IssueViewConfig -> DM.Workflow -> Action IO Bool
saveBacklogConfig (Key k) ivc w = findAndModify 
  (select ["key" =: k] projectsCollection) ["viewConfig" =: ivc, "workflow" =: w] 
  <&> either (const False) (const True)


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
    prj <- fromBSON d
    pure $ DM.projectIssues prj


filterNothing :: [Maybe a] -> [a]
filterNothing tsks = [x | Just x <- tsks]
