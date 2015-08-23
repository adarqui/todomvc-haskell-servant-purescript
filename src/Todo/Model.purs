module Model (
  module Model.Types,
  module Model.Instances,
  module Model.Functions,
  State (..),
  initialState
) where

import Prelude
import qualified Data.Map as M

import Model.Types
import Model.Instances
import Model.Functions

type State = M.Map Int Todo
initialState :: State
initialState = M.empty
