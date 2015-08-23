{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Web.TodoMVC.Servant.Shared (
  Store,
  LnAPI,
  todoAPI,
  newBigState,
  runApp,
  runApp_Maybe,
  apply2
) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Lens
import           Servant
import           Todo

data BigState = BigState {
  _todoApp :: TodoApp
}

makeLenses ''BigState

type Store = TVar BigState

type LnAPI =
       "html" :> Raw
  :<|> "dist" :> Raw
  :<|> "static" :> Raw
  -- GET /todos
  -- POST /todos , body = Todo
  -- DELETE /todos
  -- GET /todos/:todo_id
  -- DELETE /todos/:todo_id
  -- PUT /todos/:todo_id , body = Todo
  :<|> "todos" :> Get '[JSON] [Todo]
  :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] Todo
  :<|> "todos" :> Delete '[JSON] Bool
  :<|> "todos" :> Capture "todo_id" TodoId :> Get '[JSON] Todo
  :<|> "todos" :> Capture "todo_id" TodoId :> Delete '[JSON] TodoId
  :<|> "todos" :> Capture "todo_id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] Todo

todoAPI :: Proxy LnAPI
todoAPI = Proxy

-- | newBigState
--
newBigState :: IO (TVar BigState)
newBigState = newTVarIO $ BigState newTodoApp

-- | runApp
--
-- simple todo application helper
--
runApp :: MonadIO m => Store -> State TodoApp b -> EitherT ServantErr m b
runApp store cb = do
  liftIO $ atomically $ do
    v <- readTVar store
    let (a, s) = runState cb (_todoApp v)
    writeTVar store (set todoApp s v)
    return a

-- | runApp_Maybe
--
-- returns an error if our todo action returns Nothing
--
runApp_Maybe :: MonadIO m => Store -> State TodoApp (Maybe b) -> EitherT ServantErr m b
runApp_Maybe store cb = runApp store cb >>= maybe (left err400) return

-- | apply2
--
-- bleh: having some weird type errors
--
apply2 :: MonadIO m => (t -> t1 -> State TodoApp (Maybe b)) -> Store -> t -> t1 -> EitherT ServantErr m b
apply2 f s x y = runApp_Maybe s (f x y)
