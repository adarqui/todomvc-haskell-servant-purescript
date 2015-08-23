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

-- | newTodoApp
--
newTodoApp :: TodoApp
newTodoApp = TodoApp [] 0


-- | listTodos
--
listTodos :: TodoAppState [Todo]
listTodos = gets todoAppTodos >>= return


-- | addTodo
--
addTodo :: Todo -> TodoAppState Todo
addTodo todo = do
  new_id <- incrTodoAppCounter
  let
    todo' = todo { todoId = new_id }
  modify (\st -> st { todoAppTodos = (todo' : todoAppTodos st) })
  return todo'


-- | removeTodo
--
removeTodo :: TodoId -> TodoAppState (Maybe TodoId)
removeTodo tid = do
  todos <- gets todoAppTodos
  let
    e      = find (\todo -> todoId todo == tid) todos
    todos' = filter (\todo -> todoId todo /= tid) todos
  if e == Nothing
     then return Nothing
     else modify (\st -> st { todoAppTodos = todos' }) >> (return $ Just tid)


-- | updateTodo
--
updateTodo :: TodoId -> Todo -> TodoAppState (Maybe Todo)
updateTodo tid new_todo = do
  let new_todo' = new_todo { todoId = tid }
  todo <- findTodoById tid
  case todo of
    Nothing    -> return Nothing
    Just todo' -> do
      todos <- gets todoAppTodos
      let filtered = filter (\todo -> todoId todo /= tid) todos
      modify (\st -> st { todoAppTodos = new_todo' : filtered })
      return $ Just new_todo'


-- | findTodoById
--
findTodoById :: TodoId -> TodoAppState (Maybe Todo)
findTodoById tid = do
  todos <- gets todoAppTodos
  return $ find (\todo -> todoId todo == tid) todos


-- | clearTodos
--
clearTodos :: TodoAppState Bool
clearTodos = do
  modify (\st -> st { todoAppTodos = [] })
  return True


-- | incrTodoAppCounter
--
-- >>> runState (incrTodoAppCounter >> incrTodoAppCounter) newTodoApp
-- (2,TodoApp {todoAppTodos = [], todoAppCounter = 2})
--
incrTodoAppCounter :: TodoAppState TodoId
incrTodoAppCounter = do
  counter <- gets todoAppCounter
  let
    new_counter = counter + 1
  modify (\st -> st { todoAppCounter = new_counter })
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
