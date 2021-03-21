{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Storage
where

import Database.MongoDB    (Action, Document, Document, Value, access,
                            close, connect, delete, exclude, find,
                            host, insertMany, master, project, rest,
                            select, sort, (=:), Database)
import Control.Monad.Trans (liftIO)
import Control.Monad ( liftM )
import Models (Task, User)
import Data.Bson.Generic


instance ToBSON User
instance FromBSON User

instance ToBSON Task
instance FromBSON Task


databasename :: Database
databasename = "tracker"

check :: IO ()
check = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master databasename run
    close pipe
    print e

run :: Action IO ()
run = do
    clearTasks
    insertTasks []
    allTasks >>= printTasks "All tasks"
    userTasks "intey" >>= printTasks "My Tasks"
    sprintTasks  "some" >>= printTasks "Sprint tasks"

clearTasks :: Action IO ()
clearTasks = delete (select [] "tasks")

insertTasks :: [Task] -> Action IO [Value]
insertTasks = insertMany "tasks" . map toBSON

allTasks :: Action IO [Task]
allTasks = toTasks $ rest =<< find (select [] "tasks") -- {sort = ["home.city" =: 1]}

toTasks :: Action IO [Document] -> Action IO [Task]
toTasks = fmap (filterNothing . map fromBSON)
            where filterNothing tsks = [x | Just x <- tsks]

userTasks :: String -> Action IO [Task]
userTasks username = toTasks $ rest =<< find (select ["assignee" =: "National"] "tasks")

sprintTasks :: String -> Action IO [Task]
sprintTasks sprintname = toTasks $ rest =<< find (select ["sprint" =: sprintname] "tasks")

printTasks :: String -> [Task] -> Action IO ()
printTasks title tasks = liftIO $ putStrLn title >> mapM_ print tasks

