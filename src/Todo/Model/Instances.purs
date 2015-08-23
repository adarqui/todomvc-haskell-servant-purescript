module Model.Instances where

import Prelude
import Data.Array
import Data.JSON
import Data.Maybe
import Data.Tuple
import Data.Functor

import Model.Types

instance todoStateEq :: Eq TodoState where
  eq Active Active       = true
  eq Completed Completed = true
  eq _ _                 = false

instance todoEq :: Eq Todo where
  eq (Todo t1) (Todo t2) = t1.todoId == t2.todoId

instance todoOrd :: Ord Todo where
  compare (Todo t1) (Todo t2) = compare t1.todoId t2.todoId

instance showTodoState :: Show TodoState where
  show Active    = "Active"
  show Completed = "Completed"

instance todoStateFromJSON :: FromJSON TodoState where
  parseJSON (JString s) =
    case s of
      "Active"    -> return Active
      "Completed" -> return Completed
      _           -> fail "Unknown TodoState"

instance todoStateToJSON :: ToJSON TodoState where
  toJSON Active    = JString "Active"
  toJSON Completed = JString "Completed"

instance showTodo :: Show Todo where
  show (Todo {todoId=tid, todoTitle=title, todoState=state}) =
    "Todo { todoId = " ++show tid ++ ", todoTitle = \"" ++ title ++ "\", todoState = " ++ show state

instance todoFromJSON :: FromJSON Todo where
  parseJSON (JObject o) = do
    tid   <- o .: "todoId"
    title <- o .: "todoTitle"
    state <- o .: "todoState"
    return $ Todo { todoId: tid, todoTitle: title, todoState: state }
  parseJSON _ = fail "Invalid Todo"

instance todoToJSON :: ToJSON Todo where
  toJSON (Todo { todoId = tid, todoTitle = title, todoState = state }) =
    object [ "todoId" .= tid, "todoTitle" .= title, "todoState" .= state ]

instance todoAppShow :: Show TodoApp where
  show (TodoApp app) = "{ todoAppCounter: " ++ show app.todoAppCounter ++ ", todoAppTodos: " ++ show app.todoAppTodos ++ " }"
