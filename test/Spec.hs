{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import qualified Data.Text as T
import Network.HTTP.Simple
import Network.HTTP.Types (Method, methodGet, methodPost, methodPatch, methodDelete)
import Data.List (isInfixOf)
import Data.Aeson (encode, object, (.=))
import Control.Exception (bracket)
import Database.PostgreSQL.Simple
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BL8

baseUrl :: String
baseUrl = "http://localhost:3000"

-- Function to set up the database connection
setupDB :: IO Connection
setupDB = do
    conn <- connectPostgreSQL "postgresql://user:password@localhost:5432/todoapp" 
    execute_ conn "CREATE TABLE IF NOT EXISTS todos (id SERIAL PRIMARY KEY, title TEXT NOT NULL, completed BOOLEAN NOT NULL DEFAULT FALSE)"
    return conn

-- Function to tear down the database
teardownDB :: Connection -> IO ()
teardownDB conn = do
    execute_ conn "DROP TABLE IF EXISTS todos"
    close conn

-- Helper to create a request
createRequest :: Method -> String -> Request
createRequest method path = setRequestMethod method
                          $ setRequestPath (encodeUtf8 $ pack path)
                          $ setRequestHost "localhost"
                          $ setRequestPort 3000
                          $ defaultRequest

main :: IO ()
main = bracket setupDB teardownDB $ \conn -> do
    _ <- runTestTT tests
    return ()

tests :: Test
tests = TestList
    [ TestLabel "GET /todos returns an empty list" $ TestCase $ do
        let request = createRequest methodGet "/todos"
        response <- httpLBS request
        assertEqual "Status code should be 200" 200 (getResponseStatusCode response)
        assertEqual "Response should be an empty list" "[ ]" (getResponseBody response)

    , TestLabel "POST /todos adds a todo" $ TestCase $ do
        let requestBody = encode $ object ["title" .= ("New Todo" :: String)]
        let request = setRequestBodyLBS requestBody $ createRequest methodPost "/todos"
        response <- httpLBS request
        assertEqual "Status code should be 200" 200 (getResponseStatusCode response)

    , TestLabel "GET /todos returns the added todo" $ TestCase $ do
        let request = createRequest methodGet "/todos"
        response <- httpLBS request
        assertEqual "Status code should be 200" 200 (getResponseStatusCode response)
        assertBool "Response should contain the added todo" ("New Todo" `isInfixOf` (BL8.unpack $ getResponseBody response))

    , TestLabel "PATCH /todos/:id updates a todo" $ TestCase $ do
        let requestBody = encode $ object []  
        let request = setRequestBodyLBS requestBody $ createRequest methodPatch "/todos/1"
        response <- httpLBS request
        assertEqual "Status code should be 200" 200 (getResponseStatusCode response)

    , TestLabel "DELETE /todos/:id deletes a todo" $ TestCase $ do
        let request = createRequest methodDelete "/todos/1"
        response <- httpLBS request
        assertEqual "Status code should be 204" 204 (getResponseStatusCode response)
    ]
