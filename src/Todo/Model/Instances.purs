module Model.Instances where

import Prelude
import Data.Array
import Data.JSON
import Data.Maybe
import Data.Tuple
import Data.Functor

import Model.Types

instance _todoStateEq :: Eq TodoState where
  eq Active Active       = true
  eq Completed Completed = true
  eq _ _                 = false

instance todoEq :: Eq Todo where
  eq (Todo t1) (Todo t2) = t1._todoId == t2._todoId

instance todoOrd :: Ord Todo where
  compare (Todo t1) (Todo t2) = compare t1._todoId t2._todoId

instance showTodoState :: Show TodoState where
  show Active    = "Active"
  show Completed = "Completed"

instance _todoStateFromJSON :: FromJSON TodoState where
  parseJSON (JString s) =
    case s of
      "Active"    -> return Active
      "Completed" -> return Completed
      _           -> fail "Unknown TodoState"

instance _todoStateToJSON :: ToJSON TodoState where
  toJSON Active    = JString "Active"
  toJSON Completed = JString "Completed"

instance showTodo :: Show Todo where
  show (Todo {_todoId=tid, _todoTitle=title, _todoState=state}) =
    "Todo { _todoId = " ++show tid ++ ", _todoTitle = \"" ++ title ++ "\", _todoState = " ++ show state

instance todoFromJSON :: FromJSON Todo where
  parseJSON (JObject o) = do
    tid   <- o .: "_todoId"
    title <- o .: "_todoTitle"
    state <- o .: "_todoState"
    return $ Todo { _todoId: tid, _todoTitle: title, _todoState: state }
  parseJSON _ = fail "Invalid Todo"

instance todoToJSON :: ToJSON Todo where
  toJSON (Todo { _todoId = tid, _todoTitle = title, _todoState = state }) =
    object [ "_todoId" .= tid, "_todoTitle" .= title, "_todoState" .= state ]

instance todoAppShow :: Show TodoApp where
  show (TodoApp app) = "{ _todoAppCounter: " ++ show app._todoAppCounter ++ ", _todoAppTodos: " ++ show app._todoAppTodos ++ " }"
