-- {-# LANGUAGE OverloadedStrings #-}

-- import Test.Hspec
-- import Test.Hspec.Wai
-- import Test.Hspec.Wai.JSON
-- import Network.Wai.Test (defaultRequest)
-- import Network.HTTP.Types.Method (methodGet, methodPost, methodPatch, methodDelete)
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Aeson (encode, object, (.=))
-- import Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_, MVar)
-- import Web.Scotty

-- -- Mock Todo type
-- data Todo = Todo
--   { todoId      :: Int
--   , title       :: String
--   , completed   :: Bool
--   } deriving (Show, Eq)

-- -- Mock database operations
-- type MockDB = MVar [Todo]

-- mockGetTodos :: MockDB -> IO [Todo]
-- mockGetTodos mvar = readMVar mvar

-- mockAddTodo :: MockDB -> String -> IO ()
-- mockAddTodo mvar title = modifyMVar_ mvar $ \todos -> return (todos ++ [Todo (length todos + 1) title False])

-- mockUpdateTodo :: MockDB -> Int -> IO ()
-- mockUpdateTodo mvar id = modifyMVar_ mvar $ \todos -> return (map (\t -> if todoId t == id then t { completed = not (completed t) } else t) todos)

-- mockDeleteTodo :: MockDB -> Int -> IO ()
-- mockDeleteTodo mvar id = modifyMVar_ mvar $ \todos -> return (filter (\t -> todoId t /= id) todos)

-- -- Create a Scotty application with a mock database
-- app :: MockDB -> Scotty ()
-- app db = do
--     get "/todos" $ do
--         todos <- liftIO $ mockGetTodos db
--         json todos

--     post "/todos" $ do
--         titleParam <- param "title"
--         liftIO $ mockAddTodo db titleParam
--         todos <- liftIO $ mockGetTodos db
--         json todos

--     patch "/todos/:id" $ do
--         todoIdParam <- param "id"
--         liftIO $ mockUpdateTodo db todoIdParam
--         todos <- liftIO $ mockGetTodos db
--         json todos

--     delete "/todos/:id" $ do
--         todoIdParam <- param "id"
--         liftIO $ mockDeleteTodo db todoIdParam
--         todos <- liftIO $ mockGetTodos db
--         json todos

-- main :: IO ()
-- main = hspec $ do
--     describe "Todo API" $ do
--         it "returns todos" $ do
--             todosVar <- newMVar [Todo 1 "Mock Todo 1" False, Todo 2 "Mock Todo 2" True]
--             let testApp = app todosVar
--             response <- runWaiSession (HTTP.Wai.request methodGet "/todos" defaultRequest) testApp
--             response `shouldRespondWith` 200
--             response `shouldRespondWith` (encode [Todo 1 "Mock Todo 1" False, Todo 2 "Mock Todo 2" True])

--         it "adds a todo" $ do
--             todosVar <- newMVar []
--             let testApp = app todosVar
--             HTTP.Wai.request methodPost "/todos" (encode $ object ["title" .= ("New Todo" :: String)]) `shouldRespondWith` 200
--             todos <- mockGetTodos todosVar
--             length todos `shouldBe` 1
--             (title (head todos)) `shouldBe` "New Todo"

--         it "updates a todo" $ do
--             todosVar <- newMVar [Todo 1 "Mock Todo 1" False]
--             let testApp = app todosVar
--             HTTP.Wai.request methodPatch "/todos/1" defaultRequest `shouldRespondWith` 200
--             todos <- mockGetTodos todosVar
--             (completed (head todos)) `shouldBe` True

--         it "deletes a todo" $ do
--             todosVar <- newMVar [Todo 1 "Mock Todo 1" False]
--             let testApp = app todosVar
--             HTTP.Wai.request methodDelete "/todos/1" defaultRequest `shouldRespondWith` 200
--             todos <- mockGetTodos todosVar
--             length todos `shouldBe` 0