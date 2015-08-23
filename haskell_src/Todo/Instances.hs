{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Todo.Instances where

import           Data.Aeson
import           GHC.Generics
import Todo.Types

instance FromJSON Todo
instance ToJSON Todo

instance FromJSON TodoState
instance ToJSON TodoState
