module Helpers.Map (
  toArray,
  fromArray,
  elems,
  indices
) where

import Prelude
import Data.Tuple
import qualified Data.List as L
import qualified Data.Map as M

import Helpers

-- | toArray
--
-- M.toList to an array
toArray :: forall k v. M.Map k v -> Array (Tuple k v)
toArray = listToArray <<< M.toList

-- | fromArray
--
fromArray :: forall k v. (Ord k) => Array (Tuple k v) -> M.Map k v
fromArray = M.fromList <<< arrayToList

-- | elems
--
elems :: forall k v. M.Map k v -> Array v
elems = map snd <<< toArray

-- | indices
--
indices :: forall k v. M.Map k v -> Array k
indices = map fst <<< toArray
