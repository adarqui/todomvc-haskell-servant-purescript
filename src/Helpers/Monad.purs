module Helpers.Monad (
  (>>),
  maybeM
) where

import Prelude
import Control.Monad
import Data.Maybe

(>>) :: forall m a b. (Bind m) => m a -> m b -> m b
(>>) x y = x >>= const y

maybeM :: forall m a b. (Monad m) => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM def cb mb = do
  mb' <- mb
  case mb' of
       Nothing -> def
       Just a  -> cb a
