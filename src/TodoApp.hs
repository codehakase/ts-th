{-# LANGUAGE OverloadedStrings #-}

module TodoApp where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Web.Scotty
import Lucid
import Lucid.Base 
import Control.Monad.IO.Class (liftIO)
import DB

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

runApp :: IO ()
runApp = do
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