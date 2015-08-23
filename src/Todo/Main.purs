module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff, later', launchAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Data.Const (Const())
import Data.Void (Void())

import Network.HTTP.Affjax (AJAX())

import Halogen
import Halogen.Util (appendToBody)

import Model
import Component.List
import Component.Todo
import Shared

ui :: forall p.
  InstalledComponentP                   -- InstalledComponentP s s' f f' g o o' p p' =
    State                               -- s
    TodoView                            -- s'
    ListInput                           -- f
    TodoInput                           -- f'
    TodoEffects                         -- g
    (ChildF TodoPlaceholder TodoInput)  -- o
    (Const Void)                        -- o'
    TodoPlaceholder                     -- p
    p                                   -- p'
ui = install' list mkTodo

main :: Eff (HalogenEffects (ajax :: AJAX, console :: CONSOLE)) Unit
main = launchAff $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
  app.driver (action ListTodos)
