{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

--
-- really simple todo impl.
-- todo (ironically): type level safety between requests and responses
--

module Todo.App (
  newTodoApp,
  defaultTodo,
  listTodos,
  addTodo,
  removeTodo,
  updateTodo,
  setTodoActive,
  setTodoCompleted,
  setTodoState,
  findTodoById,
  findTodosByTitle,
  findActiveTodos,
  findCompletedTodos,
  findTodosByState,
  clearTodos,
  clearCompletedTodos,
  runTodoGrammar,
  incrTodoAppCounter
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Aeson
import           Data.List
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics

import Todo.Types
import Todo.Instances

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

-- | setTodoActive
--
setTodoActive :: TodoId -> TodoAppState (Maybe Todo)
setTodoActive = setTodoState Active

-- | setTodoCompleted
--
setTodoCompleted :: TodoId -> TodoAppState (Maybe Todo)
setTodoCompleted = setTodoState Completed

-- | setTodoState
--
setTodoState :: TodoState -> TodoId -> TodoAppState (Maybe Todo)
setTodoState tst tid = do
  todo <- findTodoById tid
  case todo of
    Nothing    -> return Nothing
    Just todo' -> updateTodo tid (todo' { todoState = tst })

-- | findTodoById
--
findTodoById :: TodoId -> TodoAppState (Maybe Todo)
findTodoById tid = do
  todos <- gets todoAppTodos
  return $ find (\todo -> todoId todo == tid) todos

-- | findTodosByTitle
--
findTodosByTitle :: Text -> TodoAppState [Todo]
findTodosByTitle s = do
  todos <- gets todoAppTodos
  let
    filtered = filter (\todo -> T.isInfixOf (T.toLower s) (T.toLower $ todoTitle todo)) todos
  return filtered

-- | findActiveTodos
--
findActiveTodos :: TodoAppState [Todo]
findActiveTodos = findTodosByState Active

-- | findCompletedTodos
--
findCompletedTodos :: TodoAppState [Todo]
findCompletedTodos = findTodosByState Completed

-- | findTodosByState
--
findTodosByState :: TodoState -> TodoAppState [Todo]
findTodosByState tst = do
  todos <- gets todoAppTodos
  let
    filtered = filter (\todo -> todoState todo == tst) todos
  return filtered

-- | clearTodos
--
clearTodos :: TodoAppState Bool
clearTodos = do
  modify (\st -> st { todoAppTodos = [] })
  return True

-- | clearCompletedTodos
--
clearCompletedTodos :: TodoAppState Bool
clearCompletedTodos = do
  modify (\st -> st { todoAppTodos = filter (\todo -> todoState todo /= Completed) (todoAppTodos st) })
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
runTodoGrammar (ReqSetTodoActive tid)    = RespSetTodoActive       <$> setTodoState Active tid
runTodoGrammar (ReqSetTodoCompleted tid) = RespSetTodoCompleted    <$> setTodoState Completed tid
runTodoGrammar (ReqFindTodoById tid)     = RespFindTodoById        <$> findTodoById tid
runTodoGrammar (ReqFindTodosByTitle s)   = RespFindTodosByTitle    <$> findTodosByTitle s
runTodoGrammar ReqFindActiveTodos        = RespFindActiveTodos     <$> findTodosByState Active
runTodoGrammar ReqFindCompletedTodos     = RespFindCompletedTodos  <$> findTodosByState Completed
runTodoGrammar ReqClearTodos             = RespClearTodos          <$> clearTodos
runTodoGrammar ReqClearCompletedTodos    = RespClearCompletedTodos <$> clearCompletedTodos
