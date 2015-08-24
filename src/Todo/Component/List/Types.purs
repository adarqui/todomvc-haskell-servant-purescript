module Component.List.Types (
  ListInput (..),
  ListView (..)
) where

import Model

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
