module Component.List (
  module Component.List.Types,
  list
) where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff(), runAff, launchAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Free (Free(), liftFI)
import Control.Monad.Aff.AVar (AVAR (..))
import Control.Monad.Eff.Exception (EXCEPTION (..), throwException)

import qualified Control.Monad.State as St
import qualified Control.Monad.State.Trans as St
import qualified Control.Monad.State.Class as St

import Data.Array (snoc, filter, length, range, zip)
import Data.Const (Const())
import Data.Foldable (foldl)
import Data.Functor (($>))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Void (Void())
import Data.JSON (decode, encode)
import qualified Data.Map as M

import Halogen
import Halogen.Query.StateF (StateF(), modify, gets)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E

import Model
import Component.List.Instances
import Component.List.Types
import Component.Todo
import Shared
import qualified Helpers.Map as M
import Helpers.UI (_class)
import Helpers.Monad
import Helpers.Monad.Array

import qualified Data.String as S

list :: forall p.
  ParentComponentP
    ListState
    TodoView
    ListInput
    TodoInput
    TodoEffects
    (ChildF TodoPlaceholder TodoInput)
    (Const Void)
    TodoPlaceholder
    p
list = component' render eval peek
  where

  render :: Render ListState ListInput TodoPlaceholder
  render (ListState st) = appLayout

    where

    -- | appLayout
    -- The full todo app layout, from head to toe.
    --
    appLayout =
      H.section [_class "todoapp"] [headerAndInput, todoListAndRoutes, footer]

    -- | headerAndInput
    -- The header includes our input field for adding new todos
    --
    headerAndInput =
      H.header [_class "header"] [
        H.h1_ [H.text "todos"],
        H.input [
          _class "new-todo",
          P.placeholder "What needs to be done?",
--          maybe (P.value "") P.value (Just "FIXME"),
          E.onValueChange (E.input NewTodo)
        ]
      ]

    -- | todoListAndRoutes
    -- The actual todo list, consisting of active & completed todos. Also contains the hashtag routes for active/ completed todos.
    todoListAndRoutes =
      H.section [_class "main"] [
        H.input [_class "toggle-all", P.type_ "checkbox"], -- [H.label_ [H.text "Mark all as complete"]],
          H.ul [_class "todo-list"] $ map (H.Placeholder <<< TodoPlaceholder) todosFilter,
        H.footer [_class "footer"] [
          H.span [_class "todo-count"] [H.strong_ [H.text $ show $ length $ filter isListActive $ M.elems st.todos], H.text " items left"],
          H.ul [_class "filters"] [
            H.li_ [H.a [P.href "#"] [H.text "All"]],
            H.li_ [H.a [P.href "#active"] [H.text "Active"]],
            H.li_ [H.a [P.href "#completed"] [H.text "Completed"]]
          ],
          H.button [_class "clear-completed", E.onClick (E.input_ ClearTodos)] [H.text "Clear completed"]
        ]
      ]

    -- | footer
    --
    footer =
      H.footer [_class "info"] [
        H.p_ [H.text "Double-click to edit a todo"],
        H.p_ [H.text "Created by ", H.a [P.href "https://github.com/adarqui/"] [H.text "adarqui"]],
        H.p_ [H.text "Part of ", H.a [P.href "http://todomvc.com"] [H.text "TodoMVC"]]
      ]

    -- | todosFilter
    -- Filters the todo list based on the hash routes.
    --
    todosFilter :: Array Todo
    todosFilter
      | st.view == ListViewActive    = filter isListActive $ M.elems st.todos
      | st.view == ListViewCompleted = filter isListCompleted $ M.elems st.todos
      | otherwise                    = M.elems st.todos

  eval :: Eval ListInput ListState ListInput (QueryF ListState TodoView TodoInput TodoEffects TodoPlaceholder p)

  eval (GetState next) = do
    pure next

  eval (NewTodo title next) = do
    r <- liftQuery $ liftFI $ ajaxAddTodo (defaultTodo title)
    case r of
         Nothing   -> pure next
         Just todo@(Todo t) -> modify (\(ListState st) -> ListState { todos: M.insert t._todoId todo st.todos, view: st.view }) $> next

  eval (ListTodos next) = do
    r <- liftQuery $ liftFI $ ajaxListTodos
    case r of
      Nothing -> pure next
      Just r' -> modify (\(ListState st) -> ListState { todos: foldl (\acc (todo@(Todo t)) -> M.insert t._todoId todo acc) M.empty r', view: st.view }) $> next

  eval (ClearTodos next) = do
    r <- liftQuery $ liftFI $ ajaxClearTodos
    modify (\(ListState st) -> ListState { todos: M.empty, view: st.view })
    pure next

  eval (SetView hash next) = do
    let view = handleViewChange hash
    modify (\(ListState st) -> ListState { todos: st.todos, view: view }) $> next


  peek :: Peek ListState ListInput (QueryF ListState TodoView TodoInput TodoEffects TodoPlaceholder p) (ChildF TodoPlaceholder TodoInput)
  peek (ChildF p q) = case q of

    Remove _ -> do
      t <- liftQuery $ query p (request GetTodo)
      maybe (pure unit) (modify <<< removeTodo) t

    TodoModified next -> do
      t <- liftQuery $ query p (request GetTodo)
      maybe (pure unit) (modify <<< updateTodo) t
    _ -> pure unit

removeTodo :: Todo -> ListState -> ListState
removeTodo (Todo todo) (ListState st) = ListState { todos: M.delete todo._todoId st.todos, view: st.view }

updateTodo :: Todo -> ListState -> ListState
updateTodo todo_@(Todo todo) (ListState st) = ListState { todos: M.update (const $ Just todo_) todo._todoId st.todos, view: st.view }

handleViewChange :: String -> ListView
handleViewChange "active"    = ListViewActive
handleViewChange "completed" = ListViewCompleted
handleViewChange _           = ListViewAll

isListActive :: Todo -> Boolean
isListActive (Todo todo) = todo._todoState == Active

isListCompleted :: Todo -> Boolean
isListCompleted (Todo todo) = todo._todoState == Completed
