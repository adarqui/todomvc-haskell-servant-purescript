{-# LANGUAGE DeriveGeneric     #-}

module Todo.Instances where

import           Todo.Types

mkLenses ''Todo
mkLenses ''TodoApp
