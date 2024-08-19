{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DB where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Environment (getEnv)

-- Define the Todo type
data Todo = Todo
  { todoId      :: Int
  , title       :: String
  , completed   :: Bool
  } deriving (Show, Generic)

instance FromJSON Todo
instance ToJSON Todo

-- Automatically parse a Todo from a database row
instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field

connectDB :: IO Connection
connectDB = do
  dbUrl <- getEnv "DB_URL"
  let dbUrlText = T.pack dbUrl
  let dbUrlBS = TE.encodeUtf8 dbUrlText
  conn <- connectPostgreSQL dbUrlBS
  return conn

createTablesIfNotExists :: Connection -> IO ()
createTablesIfNotExists conn = do
  let createTableSQL = 
        "CREATE TABLE IF NOT EXISTS todos (" <>
        "id SERIAL PRIMARY KEY, " <>
        "title TEXT NOT NULL, " <>
        "completed BOOLEAN NOT NULL DEFAULT FALSE" <>
        ");"
  execute_ conn createTableSQL >> return ()

getTodos :: Connection -> IO [Todo]
getTodos conn = query_ conn "SELECT id, title, completed FROM todos"

addTodo :: Connection -> String -> IO ()
addTodo conn todoTitle = execute conn "INSERT INTO todos (title) VALUES (?)" (Only todoTitle) >> return ()

updateTodo :: Connection -> Int -> IO ()
-- setting to the opposite of the completed field for each hit
updateTodo conn id = execute conn "UPDATE todos SET completed = NOT completed WHERE id = ?" (Only id) >> return ()

deleteTodo :: Connection -> Int -> IO ()
deleteTodo conn id = execute conn "DELETE FROM todos WHERE id = ?" (Only id) >> return ()