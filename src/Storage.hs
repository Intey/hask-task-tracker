{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage where

import           Control.Monad (liftM)
import           Control.Monad.Trans (liftIO)
import           Prelude hiding (lookup)
import           Control.Monad.Except
import           Data.Bson
import           Data.Bson.Generic
import           Data.Char (toUpper)
import           Data.Functor ((<&>))
import           Database.MongoDB (Action, Collection, Document, Query(project)
                                 , Select(select), cast, ensureIndex, find
                                 , findAndModify, findOne, insert, insert_, look
                                 , rest, typed, (=:))
import           Database.MongoDB.Admin (Index(Index))
import           Debug.Trace
import           Domain.InputBounds.CreateIssue
import           Domain.InputBounds.CreateUser
import qualified Domain.Interfaces as DI
import           Domain.Models (Key(Key), User(User))
import qualified Domain.Models as DM
import           Domain.Models.BacklogScreen
import           Domain.Models.Issue
import           Domain.Models.Project as Prj
import           Domain.Models.Workflow

projectsCollection :: Collection
projectsCollection = "projects"

usersCollection :: Collection
usersCollection = "users"

loginCollection :: Collection
loginCollection = "logins"

initDB :: Action IO ()
initDB = do
  ensureIndex $ Index projectsCollection ["key" =: 1] "key" True True Nothing
  ensureIndex $ Index usersCollection ["key" =: 1] "key" True True Nothing
  ensureIndex $ Index loginCollection ["key" =: 1] "key" True True Nothing

-- TODO: custom instances for (To/From)BSON. Now it's saves {"projectName": "prj", "projectKey": {"": "prj-"} }
-- nested objects for keys
instance ToBSON (Key User)

instance FromBSON (Key User)

instance ToBSON User where
  toBSON u = [ "key"
               := val
                 (let (Key k) = DM.username u
                  in k)]

instance FromBSON User where
  fromBSON d = do
    username <- lookup "username" d
    id_ <- look "_id" d
    return $ User (Key username) (show id_)

instance ToBSON (Key Issue)

instance FromBSON (Key Issue)

instance ToBSON Issue

instance FromBSON Issue

instance ToBSON (Key Project)

instance FromBSON (Key Project)

instance FromBSON IssueField

instance ToBSON IssueField

instance FromBSON IssueViewConfig

instance ToBSON IssueViewConfig

instance ToBSON Project

  -- where
  -- toBSON prj = [ "key" := val (let (Key k) = DM.projectKey prj in k)
  --              , "name" := val (DM.projectName prj)
  --              , "description" := val (DM.projectDescription prj)
  --              , "owner" := val (let (Key k) = DM.projectOwner prj in k)
  --              ]
instance FromBSON Project

  -- where
  -- fromBSON doc = do
  --   key <- lookup "key" doc
  --   name <- lookup "name" doc
  --   descr <- lookup "description" doc
  --   owner <- lookup "owner" doc
  --   return $ Project (Key key) name descr (Key owner) []
instance ToBSON Workflow

instance FromBSON Workflow

{-| saveProject updates exist project -}
-- saveProject :: Project -> Action IO ()
-- saveProject = insert_ projectsCollection . toBSON
-- TODO: createProject: save id from uuid created in functions
createProject :: Project -> Action IO (Key Project)
createProject p = do
  insert projectsCollection (toBSON p) <&> Key . show

loadProject :: Key Project -> Action IO (Maybe Project)
loadProject k = do
  doc <- findOne (select ["key" =: k] projectsCollection)
  return (doc >>= fromBSON)

createIssue :: Key Project
            -> Issue
            -> ExceptT String Action IO (Either String (Key Issue))
createIssue p (Issue k s d a r ls) = do
  result <- findAndModify
    (select ["key" =: p] projectsCollection)
    ["issues.$addToSet" =: Issue (Key $ map toUpper s) s d a r ls]
  case result of
    Left e  -> return $ Left e
    Right d -> return $ Right k

projectIssues :: Key Project -> Action IO [Issue]
projectIssues (Key k) = do
  proj <- findOne (select ["key" =: k] projectsCollection)
  case parseProjectIssues proj of
    Just is -> pure is
    Nothing -> pure []

loadProjectName :: Key Project -> Action IO (Maybe String)
loadProjectName (Key k) = do
  proj <- findOne
    (select ["key" =: k] projectsCollection) { Database.MongoDB.project =
                                                 ["name" =: 1, "_id" =: 0]
                                             }
  return $ proj >>= look "name" >>= cast

loadIssueViewConfig :: Key Project -> Action IO (Maybe IssueViewConfig)
loadIssueViewConfig (Key k) = do
  proj <- findOne
    (select
       ["key" =: k]
       projectsCollection) { Database.MongoDB.project = ["viewConfig" =: 1] }
  return $ proj >>= look "viewConfig" >>= cast

-- saveBacklogConfig
--   :: Key Project -> IssueViewConfig -> Workflow -> Action IO Bool
-- saveBacklogConfig (Key k) ivc w = findAndModify
--   (select ["key" =: k] projectsCollection)
--   ["viewConfig" =: ivc, "workflow" =: w]
--   <&> either (const False) (const True)
toIssue :: Action IO [Document] -> Action IO [Issue]
toIssue = fmap (filterNothing . map fromBSON)

toUser :: Action IO [Document] -> Action IO [User]
toUser = fmap
  (\u -> trace ("toUser with " ++ show u) filterNothing $ map fromBSON u)

allUsers :: Action IO [User]
allUsers = toUser . rest =<< find (select [] usersCollection)

{- | Saves new user
TODO: Should accepts Input-Bound Struct "NewUser" and return User key
TODO: return user key
-}
insertUser :: CreateUserSchema -> Action IO (Key User)
insertUser u@(CreateUserSchema username) = do
  -- let u = User username
  insert_ usersCollection $ toBSON u
  return $ Key username

loadUser :: String -> Action IO (Maybe User)
loadUser login = do
  res <- findOne (select ["key" =: login] usersCollection)
  pure $ res >>= fromBSON

parseProjectIssues :: Maybe Document -> Maybe [Issue]
parseProjectIssues doc = do
  d <- doc
  prj <- fromBSON d
  pure $ Prj.issues prj

filterNothing :: [Maybe a] -> [a]
filterNothing tsks = [x | Just x <- tsks]
