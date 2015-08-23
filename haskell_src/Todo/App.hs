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
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.List
import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Generics hiding (to)
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
    todo' = set todoId new_id todo
  todoAppTodos %= M.insert new_id todo'
  return todo'


-- | removeTodo
--
removeTodo :: TodoId -> TodoAppState (Maybe TodoId)
removeTodo tid = do
  -- trying to get this 'use' to work like this 'view':
  -- view (C.to (M.lookup 1)) M.empty
  e <- use (to (M.lookup tid)) todoAppTodos
  maybe (return Nothing) (const del) e
  where
    del = todoAppTodos %= M.delete tid >> (pure . pure) tid

{-
  todos <- gets _todoAppTodos
  let
    e = M.lookup tid todos
  case e of
    Just _ -> do
      todoAppTodos %= M.delete tid
      return $ Just tid
    _      -> pure Nothing
    -}
{-
  todos <- gets _todoAppTodos
  let
    e = M.lookup tid todos
  case e of
    Just _ -> modify (\st -> st { _todoAppTodos = M.delete tid (_todoAppTodos st) }) *> (pure . pure) tid
    _      -> pure Nothing
    -}


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
findTodoById tid =  M.lookup tid <$> gets _todoAppTodos


-- | clearTodos
--
clearTodos :: TodoAppState Bool
clearTodos = do
  todoAppTodos .= M.empty
  pure True


-- | incrTodoAppCounter
--
-- >>> runState (incrTodoAppCounter >> incrTodoAppCounter) newTodoApp
-- (2,TodoApp {todoAppTodos = [], todoAppCounter = 2})
--
incrTodoAppCounter :: TodoAppState TodoId
incrTodoAppCounter = do
  todoAppCounter += 1
  gets _todoAppCounter


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
