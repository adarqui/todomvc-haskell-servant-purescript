module Helpers.UI (
  _class
)  where

import Prelude

import Halogen.HTML.Core (className)
import Halogen.HTML.Properties (class_)

-- | _class
--
_class = class_ <<< className
