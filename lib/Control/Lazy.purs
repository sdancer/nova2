module Control.Lazy where

import Prelude

-- | Type class for lazily-evaluated data structures
-- | Minimal implementation for defer function used in recursive parsers
class Lazy l where
  defer :: (Unit -> l) -> l

-- Note: The 'defer' class method will be exported when instances provide it
-- For type checking purposes, we also provide this as a typed binding
