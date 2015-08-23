{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Types (
  Todo (..),
  TodoActionRequest (..),
  TodoActionResponse (..),
  TodoState (..),
  TodoApp (..),
  TodoAppState,
  TodoId
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Aeson
import           Data.List
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics

type TodoId = Int

data Todo = Todo {
  todoId    :: TodoId,
  todoTitle :: Text,
  todoState :: TodoState
} deriving (Show, Eq, Ord, Generic)

data TodoActionRequest
  = ReqListTodos
  | ReqAddTodo Todo
  | ReqRemoveTodo TodoId
  | ReqUpdateTodo TodoId Todo
  | ReqSetTodoActive TodoId
  | ReqSetTodoCompleted TodoId
  | ReqFindTodoById TodoId
  | ReqFindTodosByTitle Text
  | ReqFindActiveTodos
  | ReqFindCompletedTodos
  | ReqClearTodos
  | ReqClearCompletedTodos
  deriving (Show, Eq, Ord, Generic)

data TodoActionResponse
  = RespListTodos [Todo]
  | RespAddTodo (Maybe Todo)
  | RespRemoveTodo (Maybe TodoId)
  | RespUpdateTodo (Maybe Todo)
  | RespSetTodoActive (Maybe Todo)
  | RespSetTodoCompleted (Maybe Todo)
  | RespFindTodoById (Maybe Todo)
  | RespFindTodosByTitle [Todo]
  | RespFindActiveTodos [Todo]
  | RespFindCompletedTodos [Todo]
  | RespClearTodos Bool
  | RespClearCompletedTodos Bool
  deriving (Show, Eq, Ord, Generic)

data TodoState
  = Active
  | Completed
  deriving (Show, Eq, Ord, Generic)

data TodoApp = TodoApp {
  todoAppTodos   :: [Todo],
  todoAppCounter :: TodoId
} deriving (Show, Eq, Ord, Generic)

type TodoAppState a = State TodoApp a
