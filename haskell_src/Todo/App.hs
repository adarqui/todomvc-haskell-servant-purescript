{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.App (
  newTodoApp,
  defaultTodo,
  listTodos,
  addTodo,
  removeTodo,
  updateTodo,
  findTodoById,
  clearTodos,
  runTodoGrammar,
  incrTodoAppCounter
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Aeson
import           Data.List
import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Generics
import           Todo.Instances
import           Todo.Types
import qualified Data.Map as M

-- | newTodoApp
--
newTodoApp :: TodoApp
newTodoApp = TodoApp M.empty 0


-- | listTodos
--
listTodos :: TodoAppState [Todo]
listTodos = gets _todoAppTodos >>= return . M.elems


-- | addTodo
--
addTodo :: Todo -> TodoAppState Todo
addTodo todo = do
  new_id <- incrTodoAppCounter
  let
    todo' = todo { _todoId = new_id }
  modify (\st -> st { _todoAppTodos = M.insert new_id todo' (_todoAppTodos st) })
  return todo'


-- | removeTodo
--
removeTodo :: TodoId -> TodoAppState (Maybe TodoId)
removeTodo tid = do
  todos <- gets _todoAppTodos
  let
    e = M.lookup tid todos
  case e of
    Just _ -> modify (\st -> st { _todoAppTodos = M.delete tid (_todoAppTodos st) }) *> (pure . pure) tid
    _      -> pure Nothing


-- | updateTodo
--
updateTodo :: TodoId -> Todo -> TodoAppState (Maybe Todo)
updateTodo tid new_todo = do
  let
    new_todo' = new_todo { _todoId = tid }
  todo <- findTodoById tid
  case todo of
    Just todo' -> modify (\st -> st { _todoAppTodos = M.update (const $ pure new_todo') tid (_todoAppTodos st) }) *> (pure . pure) new_todo'
    _          -> pure Nothing


-- | findTodoById
--
findTodoById :: TodoId -> TodoAppState (Maybe Todo)
findTodoById tid = M.lookup tid <$> gets _todoAppTodos


-- | clearTodos
--
clearTodos :: TodoAppState Bool
clearTodos = modify (\st -> st { _todoAppTodos = M.empty }) *> pure True


-- | incrTodoAppCounter
--
-- >>> runState (incrTodoAppCounter >> incrTodoAppCounter) newTodoApp
-- (2,TodoApp {todoAppTodos = [], todoAppCounter = 2})
--
incrTodoAppCounter :: TodoAppState TodoId
incrTodoAppCounter = do
  counter <- gets _todoAppCounter
  let
    new_counter = counter + 1
  modify (\st -> st { _todoAppCounter = new_counter })
  return new_counter


-- | defaultTodo
--
-- >>> defaultTodo "hi!"
-- Todo {todoId = 0, todoTitle = "hi!", todoState = Active}
--
defaultTodo :: Text -> Todo
defaultTodo title = Todo 0 title Active


-- | runTodoGrammar
--
-- our todo application grammar in its entirety.
--
runTodoGrammar :: TodoActionRequest -> TodoAppState TodoActionResponse
runTodoGrammar ReqListTodos              = RespListTodos           <$> listTodos
runTodoGrammar (ReqAddTodo todo)         = (RespAddTodo . Just)    <$> addTodo todo
runTodoGrammar (ReqRemoveTodo tid)       = RespRemoveTodo          <$> removeTodo tid
runTodoGrammar (ReqUpdateTodo tid todo)  = RespUpdateTodo          <$> updateTodo tid todo
runTodoGrammar ReqClearTodos             = RespClearTodos          <$> clearTodos
