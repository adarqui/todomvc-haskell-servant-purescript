module Helpers (
  arrayToList,
  listToArray
) where

import Prelude
import Data.Foldable
import qualified Data.Array as A
import qualified Data.List as L

-- | arrayToList
--
arrayToList :: forall a. Array a -> L.List a
arrayToList = L.reverse <<< foldl (\acc x -> x L.: acc) L.Nil

-- | listToArray
--
listToArray :: forall a. L.List a -> Array a
listToArray = A.reverse <<< foldl (\acc x -> x A.: acc) []
