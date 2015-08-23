module Model.Types (
  TodoId (..),
  TodoState (..),
  Todo (..),
  TodoActionRequest (..),
  TodoActionResponse (..),
  TodoApp (..)
) where

import Prelude
import Data.Maybe
import qualified Data.Map as M

-- | TodoId
--
type TodoId = Int

-- | TodoState
--
data TodoState
  = Active
  | Completed

-- | Todo
--
data Todo = Todo {
  todoId    :: TodoId,
  todoTitle :: String,
  todoState :: TodoState
}

-- | TodoActionRequest
--
-- Available Todo Requests
--
data TodoActionRequest
  = ReqListTodos
  | ReqAddTodo Todo
  | ReqRemoveTodo TodoId
  | ReqUpdateTodo TodoId Todo
  | ReqFindTodoById TodoId
  | ReqClearTodos
  | ReqNoOp

-- | TodoActionResponse
--
-- Possible Todo Responses
--
data TodoActionResponse
  = RespListTodos (Array Todo)
  | RespAddTodo Todo
  | RespRemoveTodo (Maybe TodoId)
  | RespUpdateTodo (Maybe Todo)
  | RespFindTodoById (Maybe Todo)
  | RespClearTodos Int
  | RespNoOp

-- | An internal todo application
--
data TodoApp = TodoApp {
  todoAppCounter :: TodoId,
  todoAppTodos   :: M.Map TodoId Todo
}
