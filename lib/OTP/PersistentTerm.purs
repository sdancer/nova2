-- | Persistent Term - global read-optimized storage for Erlang/BEAM
module OTP.PersistentTerm where

import Data.Maybe (Maybe(..))

-- | Get a value by key, returns Nothing if not found
get :: forall v. String -> Maybe v
get key = getImpl key

foreign import getImpl :: forall v. String -> Maybe v
  = "let <Key> = call 'erlang':'binary_to_atom'($0) in case call 'persistent_term':'get'(Key, 'undefined') of <'undefined'> when 'true' -> 'Nothing' <V> when 'true' -> {'Just', V} end"

-- | Get a value by key with a default
getWithDefault :: forall v. String -> v -> v
getWithDefault key def = getWithDefaultImpl key def

foreign import getWithDefaultImpl :: forall v. String -> v -> v
  = "let <Key> = call 'erlang':'binary_to_atom'($0) in call 'persistent_term':'get'(Key, $1)"

-- | Put a value by key
put :: forall v. String -> v -> Unit
put key val = putImpl key val

foreign import putImpl :: forall v. String -> v -> Unit
  = "let <Key> = call 'erlang':'binary_to_atom'($0) in let <_> = call 'persistent_term':'put'(Key, $1) in 'unit'"

-- | Erase a key
erase :: String -> Unit
erase key = eraseImpl key

foreign import eraseImpl :: String -> Unit
  = "let <Key> = call 'erlang':'binary_to_atom'($0) in let <_> = call 'persistent_term':'erase'(Key) in 'unit'"
