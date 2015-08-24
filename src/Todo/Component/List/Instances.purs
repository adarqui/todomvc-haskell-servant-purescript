module Component.List.Instances where

import Prelude
import Component.List.Types

instance listViewEq :: Eq ListView where
  eq ListViewAll ListViewAll             = true
  eq ListViewActive ListViewActive       = true
  eq ListViewCompleted ListViewCompleted = true
  eq _                 _                 = false
