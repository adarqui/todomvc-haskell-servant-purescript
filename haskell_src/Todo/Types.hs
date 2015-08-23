{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Todo.Types (
  Todo (..),
  TodoActionRequest (..),
  TodoActionResponse (..),
  TodoState (..),
  TodoApp (..),
  TodoAppState,
  TodoId,
  todoId,
  todoTitle,
  todoState,
  todoAppCounter,
  todoAppTodos
) where


import           Control.Lens
import           Control.Monad.State
import           Data.Aeson
import qualified Data.Map            as M
import           Data.Text           (Text)
import           GHC.Generics

type TodoId = Int

data TodoState
  = Active
  | Completed
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TodoState
instance ToJSON TodoState

data Todo = Todo {
  _todoId    :: TodoId,
  _todoTitle :: Text,
  _todoState :: TodoState
} deriving (Show, Eq, Ord, Generic)

makeLenses ''Todo

instance FromJSON Todo
instance ToJSON Todo

data TodoActionRequest
  = ReqListTodos
  | ReqAddTodo Todo
  | ReqRemoveTodo TodoId
  | ReqUpdateTodo TodoId Todo
  | ReqFindTodoById TodoId
  | ReqClearTodos
  deriving (Show, Eq, Ord, Generic)

data TodoActionResponse
  = RespListTodos [Todo]
  | RespAddTodo (Maybe Todo)
  | RespRemoveTodo (Maybe TodoId)
  | RespUpdateTodo (Maybe Todo)
  | RespFindTodoById (Maybe Todo)
  | RespClearTodos Bool
  deriving (Show, Eq, Ord, Generic)

data TodoApp = TodoApp {
  _todoAppTodos   :: M.Map TodoId Todo,
  _todoAppCounter :: TodoId
} deriving (Show, Eq, Ord, Generic)

makeLenses ''TodoApp

type TodoAppState a = State TodoApp a
