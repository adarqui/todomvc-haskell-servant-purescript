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

import           Control.Lens
import           Control.Monad.State.Lazy
import qualified Data.Map                 as M
import           Data.Text                (Text)
import           Todo.Types

-- | newTodoApp
--
newTodoApp :: TodoApp
newTodoApp = TodoApp M.empty 0


-- | listTodos
--
listTodos :: TodoAppState [Todo]
listTodos = use (todoAppTodos . to M.elems)


-- | addTodo
--
addTodo :: Todo -> TodoAppState Todo
addTodo todo = do
  new_id <- incrTodoAppCounter
  let new_todo = set todoId new_id todo
  todoAppTodos %= M.insert new_id new_todo
  pure new_todo


-- | removeTodo
--
removeTodo :: TodoId -> TodoAppState (Maybe TodoId)
removeTodo tid = do
  e <- use (todoAppTodos . to (M.lookup tid))
  maybe (pure Nothing) (const del) e

  where
  del = todoAppTodos %= M.delete tid >> (pure . pure) tid


-- | updateTodo
--
updateTodo :: TodoId -> Todo -> TodoAppState (Maybe Todo)
updateTodo tid updated_todo = do
  todo <- findTodoById tid
  maybe (pure Nothing) (const update) todo

  where
  new_todo = set todoId tid updated_todo
  update   = do
    todoAppTodos %= M.update (const $ pure new_todo) tid
    (pure . pure) new_todo


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
incrTodoAppCounter :: TodoAppState TodoId
incrTodoAppCounter = do
  todoAppCounter += 1
  gets _todoAppCounter


-- | defaultTodo
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
runTodoGrammar (ReqFindTodoById tid)     = RespFindTodoById        <$> findTodoById tid
runTodoGrammar ReqClearTodos             = RespClearTodos          <$> clearTodos
