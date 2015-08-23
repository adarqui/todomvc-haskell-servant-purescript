module Component.List (
  ListInput (..),
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

import Network.HTTP.Affjax
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader

import Model
import Component.Todo
import Shared
import qualified Helpers.Map as M
import Helpers.UI (_class)
import Helpers.Monad
import Helpers.Monad.Array

import qualified Data.String as S

data ListInput a
  = NewTodo String a
  | ListTodos a
  | ListedTodos (Array Todo) a
  | SetView a
  | GetState a

list :: forall p.
  ParentComponentP
    State
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

  render :: Render State ListInput TodoPlaceholder
  render st = appLayout

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
--        H.ul [_class "todo-list"] $ map (H.Placeholder <<< TodoPlaceholder) todosFilter
          H.ul [_class "todo-list"] $ map (H.Placeholder <<< TodoPlaceholder) (M.elems st),
        H.footer [_class "footer"] [
--          H.span [_class "todo-count"] [H.strong_ [H.text $ show $ length $ run listActiveTodos], H.text " items left"],
          H.ul [_class "filters"] [
            H.li_ [H.a [P.href "#"] [H.text "All"]],
            H.li_ [H.a [P.href "#active"] [H.text "Active"]],
            H.li_ [H.a [P.href "#completed"] [H.text "Completed"]]
          ]
--          H.button [_class "clear-completed", E.onClick (const $ pure (handleClearCompleted $ run listCompletedTodos))] [H.text "Clear completed"]
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

  eval :: Eval ListInput State ListInput (QueryF State TodoView TodoInput TodoEffects TodoPlaceholder p)

  eval (GetState next) = do
    pure next

  eval (NewTodo title next) = do
    r <- liftQuery $ liftFI $ ajaxAddTodo (defaultTodo title)
    case r of
         Nothing   -> pure next
         Just todo@(Todo t) -> modify (\st -> M.insert t.todoId todo st) $> next

  eval (ListTodos next) = do
    r <- liftQuery $ liftFI $ ajaxListTodos
    case r of
      Nothing -> pure next
      Just r' -> modify (\_ -> foldl (\acc (todo@(Todo t)) -> M.insert t.todoId todo acc) M.empty r') $> next

  peek :: Peek State ListInput (QueryF State TodoView TodoInput TodoEffects TodoPlaceholder p) (ChildF TodoPlaceholder TodoInput)
  peek (ChildF p q) = case q of

    Remove _ -> do
      t <- liftQuery $ query p (request GetTodo)
      maybe (pure unit) (modify <<< removeTodo) t

    TodoModified next -> do
      t <- liftQuery $ query p (request GetTodo)
      maybe (pure unit) (modify <<< updateTodo) t
    _ -> pure unit

removeTodo :: Todo -> State -> State
removeTodo (Todo todo) st = M.delete todo.todoId st

updateTodo :: Todo -> State -> State
updateTodo todo_@(Todo todo) st = M.update (const $ Just todo_) todo.todoId st

-- | AJAX: List Todos
ajaxListTodos :: forall eff. Aff (ajax :: AJAX | eff) (Maybe (Array Todo))
ajaxListTodos = do
  res <- get baseURL
  return $ decode res.response

ajaxAddTodo :: forall eff. Todo -> Aff (ajax :: AJAX | eff) (Maybe Todo)
ajaxAddTodo todo = do
  res <- affjax $ defaultRequest { method = POST, url = baseURL, content = Just (encode todo), headers = [ContentType applicationJSON] }
  return $ decode res.response
