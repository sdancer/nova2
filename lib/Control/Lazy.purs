module Control.Lazy where

import Prelude

-- | Type class for lazily-evaluated data structures
-- | Minimal implementation for defer function used in recursive parsers
class Lazy l where
  defer :: (Unit -> l) -> l

-- | Standalone defer function for type checking purposes
-- | The actual implementation comes from type class instances
defer :: forall l. (Unit -> l) -> l
defer f = deferImpl f

foreign import deferImpl :: forall l. (Unit -> l) -> l = "apply $0('unit')"
