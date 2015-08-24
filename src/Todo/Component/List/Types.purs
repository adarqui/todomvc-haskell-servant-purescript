module Component.List.Types (
  ListInput (..),
  ListView (..),
  ListState (..),
  initialState
) where

import Model
import qualified Data.Map as M

data ListInput a
  = NewTodo String a
  | ListTodos a
  | ListedTodos (Array Todo) a
  | SetView String a
  | GetState a

data ListView
  = ListViewAll
  | ListViewActive
  | ListViewCompleted

data ListState = ListState {
  todos :: M.Map Int Todo,
  view  :: ListView
}

initialState :: ListState
initialState = ListState { todos: M.empty, view: ListViewAll }
