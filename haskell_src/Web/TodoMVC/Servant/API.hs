{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Web.TodoMVC.Servant.API (
  runServer
) where

import           Network.Wai
import           Network.Wai.Handler.Warp   hiding (Connection)
import           Servant
import           Todo
import           Web.TodoMVC.Servant.Shared

-- | server
--
server :: Store -> Server LnAPI
server store =
       serveDirectory "./html"
  :<|> serveDirectory "./dist"
  :<|> serveDirectory "./bower_components"
  :<|> runApp store listTodos
  :<|> runApp store . addTodo
  :<|> runApp store clearTodos
  :<|> runApp_Maybe store . findTodoById
  :<|> runApp_Maybe store . removeTodo
  :<|> apply2 updateTodo store -- bleh

-- | app
--
app :: Store -> Application
app store = serve todoAPI $ server store

-- | runServer
--
-- runs the API servers on:
-- http://localhost:1080
--
runServer :: IO ()
runServer = do
  store <- newBigState
  run 1080 $ app store
