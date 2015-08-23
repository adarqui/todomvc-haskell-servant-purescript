module Shared (
  TodoEffects (..),
  baseURL
) where

import Prelude
import DOM (DOM())
import Halogen
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Console (CONSOLE())
import Network.HTTP.Affjax (AJAX())

type TodoEffects = Aff (HalogenEffects (ajax :: AJAX, console :: CONSOLE))

baseURL :: String
baseURL = "/todos/"
