{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Storage
(
    allTasks,
    clearTasks,
    insertTask,
    userTasks,
    sprintTasks,
    allUsers,
    insertUser,
    loginCollection,
    toUser,
    loadUser
)
where

import           Control.Monad       (liftM)
import           Control.Monad.Trans (liftIO)
import           Data.Bson.Generic
import           Database.MongoDB hiding (allUsers)
import           Models

tasksCollection :: Collection
tasksCollection = "tasks"

usersCollection :: Collection
usersCollection = "users"

loginCollection :: Collection
loginCollection = "logins"

instance ToBSON User
instance FromBSON User

instance ToBSON Task
instance FromBSON Task


clearTasks :: Action IO ()
clearTasks = delete (select [] tasksCollection)

insertTask :: Task -> Action IO Value
insertTask = insert tasksCollection . toBSON

allTasks :: Action IO [Task]
allTasks = toTasks $ rest =<< find (select [] tasksCollection) -- {sort = ["home.city" =: 1]}

userTasks :: String -> Action IO [Task]
userTasks username = toTasks $ rest =<< find (select ["assignee" =: "National"] tasksCollection)

sprintTasks :: String -> Action IO [Task]
sprintTasks sprintname = toTasks $ rest =<< find (select ["sprint" =: sprintname] tasksCollection)

allUsers :: Action IO [User]
allUsers = toUser $ rest =<< find (select [] usersCollection)

insertUser :: User -> Action IO ()
insertUser = insert_ usersCollection . toBSON


loadUser :: String -> Action IO (Maybe User)
loadUser login = do
    res <- findOne (select ["username" =: login] "users")
    pure $ res >>= fromBSON


toTasks :: Action IO [Document] -> Action IO [Task]
toTasks = fmap (filterNothing . map fromBSON)

toUser :: Action IO [Document] -> Action IO [User]
toUser = fmap (filterNothing . map fromBSON)

filterNothing :: [Maybe a] -> [a]
filterNothing tsks = [x | Just x <- tsks]

printTasks :: String -> [Task] -> Action IO ()
printTasks title tasks = liftIO $ putStrLn title >> mapM_ print tasks
