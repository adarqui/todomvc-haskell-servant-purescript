module Model.Ajax (
  ajaxListTodos,
  ajaxAddTodo,
  ajaxRemoveTodo,
  ajaxUpdateTodo,
  ajaxClearTodos
) where

import Prelude

import Control.Monad.Aff (Aff(), runAff, launchAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.JSON (decode, encode)

import Network.HTTP.Affjax
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader

import Shared
import Model.Types
import Model.Instances

ajaxListTodos :: forall eff. Aff (ajax :: AJAX | eff) (Maybe (Array Todo))
ajaxListTodos = do
  res <- get baseURL
  return $ decode res.response

ajaxAddTodo :: forall eff. Todo -> Aff (ajax :: AJAX | eff) (Maybe Todo)
ajaxAddTodo todo = do
  res <- affjax $ defaultRequest { method = POST, url = baseURL, content = Just (encode todo), headers = [ContentType applicationJSON] }
  return $ decode res.response

ajaxUpdateTodo :: forall eff. Todo -> Aff (ajax :: AJAX | eff) (Maybe Todo)
ajaxUpdateTodo todo@(Todo t) = do
  res <- affjax $ defaultRequest { method = PUT, url = (baseURL ++ show t._todoId), content = Just (encode todo), headers = [ContentType applicationJSON] }
  return $ decode res.response

ajaxRemoveTodo :: forall eff. Todo -> Aff (ajax :: AJAX | eff) Unit
ajaxRemoveTodo todo@(Todo t) = do
  res <- affjax $ defaultRequest { method = DELETE, url = (baseURL ++ show t._todoId) }
  liftEff $ log res.response
  return unit

ajaxClearTodos :: forall eff. Aff (ajax :: AJAX | eff) Unit
ajaxClearTodos = do
  res <- affjax $ defaultRequest { method = DELETE, url = baseURL }
  liftEff $ log res.response
  return unit
