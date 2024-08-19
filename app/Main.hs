{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Web.Scotty
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Lucid
import Lucid.Base 
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
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

-- Define functions to create htmx attributes
hxPatch_ :: T.Text -> Attribute
hxPatch_ url = makeAttribute "hx-patch" url

hxDelete_ :: T.Text -> Attribute
hxDelete_ url = makeAttribute "hx-delete" url

hxTrigger_ :: T.Text -> Attribute
hxTrigger_ event = makeAttribute "hx-trigger" event

hxSwap_ :: T.Text -> Attribute
hxSwap_ item = makeAttribute "hx-swap" item

hxTarget_ :: T.Text -> Attribute
hxTarget_ target = makeAttribute "hx-target" target

todoHtml :: Todo -> Html ()
todoHtml todo = li_ [class_ "todo-item flex items-center justify-between p-3 bg-gray-100 rounded-lg shadow-sm"] $ do
  span_ [class_ "flex items-center space-x-2"] $ do
    if completed todo
      then del_ $ toHtml $ title todo
      else toHtml $ title todo
  div_ [class_ "flex space-x-2"] $ do
    button_ [ class_ "bg-green-500 text-white px-3 py-1 rounded-lg hover:bg-green-600 focus:outline-none focus:ring-2 focus:ring-green-500"
            , hxPatch_ (T.pack $ "/todos/" ++ show (todoId todo))
            , hxTrigger_ "click"
            , hxTarget_ "#todo-list"
            , hxSwap_ "innerHTML"
            , name_ "completed"
            ] "Done"
    button_ [ class_ "bg-red-500 text-white px-3 py-1 rounded-lg hover:bg-red-600 focus:outline-none focus:ring-2 focus:ring-red-500"
            , hxDelete_ (T.pack $ "/todos/" ++ show (todoId todo))
            , hxTrigger_ "click"
            , hxTarget_ "#todo-list"
            , hxSwap_ "innerHTML"
            ] "Delete"

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

-- DB query definitions
getTodos :: Connection -> IO [Todo]
getTodos conn = query_ conn "SELECT id, title, completed FROM todos"

addTodo :: Connection -> String -> IO ()
addTodo conn todoTitle = execute conn "INSERT INTO todos (title) VALUES (?)" (Only todoTitle) >> return ()

updateTodo :: Connection -> Int -> IO ()
-- setting to the opposite of the completed field for each hit
updateTodo conn id = execute conn "UPDATE todos SET completed = NOT completed WHERE id = ?" (Only id) >> return ()

deleteTodo :: Connection -> Int -> IO ()
deleteTodo conn id = execute conn "DELETE FROM todos WHERE id = ?" (Only id) >> return ()

main :: IO ()
main = do
  conn <- connectDB
  createTablesIfNotExists conn

  scotty 3000 $ do
    get "/" $ file "./static/index.html"

    get "/todos" $ do
      todos <- liftIO $ getTodos conn
      html . renderText $ ul_ [id_ "todo-list", class_ "space-y-4"] $ mapM_ todoHtml todos

    post "/todos" $ do
      titleParam <- formParam "title"
      liftIO $ addTodo conn titleParam
      todos <- liftIO $ getTodos conn
      html . renderText $ ul_ [id_ "todo-list", class_ "space-y-4"] $ mapM_ todoHtml todos

    patch "/todos/:id" $ do
      todoIdParam <- captureParam "id"
      liftIO $ updateTodo conn todoIdParam
      todos <- liftIO $ getTodos conn
      html . renderText $ ul_ [id_ "todo-list", class_ "space-y-4"] $ mapM_ todoHtml todos

    delete "/todos/:id" $ do
      todoIdParam <- captureParam "id"
      liftIO $ deleteTodo conn todoIdParam
      todos <- liftIO $ getTodos conn
      html . renderText $ ul_ [id_ "todo-list", class_ "space-y-4"] $ mapM_ todoHtml todos
