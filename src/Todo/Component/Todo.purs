module Component.Todo (
  TodoView (),
  TodoViewMode (),
  TodoInput (..),
  TodoPlaceholder (..),
  mkTodo,
  todo
) where

import Prelude

import Data.Functor (($>))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe)
import Data.JSON (encode, decode)

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Free (Free(), liftFI)
import Network.HTTP.Affjax (AJAX(), get)

import Halogen
import Halogen.Query.StateF (modify, gets, get)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E

import Network.HTTP.Affjax (AJAX(), affjax, defaultRequest)
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader

import Data.String

import Model
import Shared
import Helpers.UI

data TodoInput a
  = ToggleCompleted a
  | UpdateTitle String a
  | Remove a
  | SetTvm TodoViewMode a
  | GetTodo (Todo -> a)
  | UpdateTodo a
  | TodoModified a

data TodoPlaceholder = TodoPlaceholder Todo

instance eqTodoPlaceholder :: Eq TodoPlaceholder where
  eq (TodoPlaceholder x) (TodoPlaceholder y) = x == y

instance ordTodoPlaceholder :: Ord TodoPlaceholder where
  compare (TodoPlaceholder x) (TodoPlaceholder y) = compare x y

data TodoViewMode
  = TvmEdit
  | TvmView

instance tvmEq :: Eq TodoViewMode where
  eq TvmEdit TvmEdit = true
  eq TvmView TvmView = true
  eq _       _       = false

data TodoView = TodoView {
  tvTodo :: Todo,
  tvMode :: TodoViewMode
}

mkTodo :: forall p. TodoPlaceholder -> ComponentState TodoView TodoInput TodoEffects p
mkTodo (TodoPlaceholder t) =
  Tuple
    todo
    (TodoView { tvTodo: t, tvMode: TvmView })

todo :: forall p. Component TodoView TodoInput TodoEffects p
todo = component render eval
  where

  render :: Render TodoView TodoInput p
  render tv_@(TodoView tv) =
    let
      t = todoFromObj $ tv.tvTodo
      v = H.label [] [H.text t._todoTitle]
    in
    H.li [if t._todoState == Completed then _class "completed" else _class "active"] [
      H.div [_class "view"] [
        H.input [_class "toggle", P.type_ "checkbox", P.checked (t._todoState == Completed), E.onChange (E.input_ ToggleCompleted)],
        case tv.tvMode of
           TvmView -> H.label [E.onClick (E.input_ (SetTvm TvmEdit))] [H.text t._todoTitle]
           TvmEdit -> H.input [_class "new-todo", P.value t._todoTitle, E.onValueChange (E.input UpdateTitle), E.onFocusOut (E.input_ (SetTvm TvmView))],
        H.button [_class "destroy", E.onClick (E.input_ Remove)] []
      ],
      H.input [_class "edit", P.value t._todoTitle]
    ]

  eval :: Eval TodoInput TodoView TodoInput TodoEffects

  eval (ToggleCompleted next) = do
    modify (\(TodoView tv) -> TodoView { tvTodo: toggleTodoState' tv.tvTodo, tvMode: tv.tvMode })
    eval (UpdateTodo next)

  eval (UpdateTitle title next) = do
    modify (\(TodoView tv) -> TodoView { tvTodo: todoUpdateTitle title tv.tvTodo, tvMode: tv.tvMode })
    eval (UpdateTodo next)
    eval (SetTvm TvmView next)

  eval (UpdateTodo next) = do
    (TodoView tv) <- gets id
    liftFI $ ajaxUpdateTodo tv.tvTodo
    eval (TodoModified next)

  eval (Remove next) = do
    (TodoView tv) <- gets id
    liftFI $ ajaxRemoveTodo tv.tvTodo
    pure next

  eval (SetTvm mode next) = do
    modify (\(TodoView tv) -> TodoView { tvTodo: tv.tvTodo, tvMode: mode })
    pure next

  eval (GetTodo next) = do
    (TodoView t) <- gets id
    pure (next t.tvTodo)
