-- | JSON encoding/decoding for Nova
module Data.Json where

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

-- | Opaque JSON value type
type JsonValue = Unit

-- | Decode a JSON string, returns Nothing on parse error
decode :: String -> Maybe JsonValue
decode str = decodeImpl str

foreign import decodeImpl :: String -> Maybe JsonValue
  = "case catch call 'json':'decode'($0) of <{'EXIT', _}> when 'true' -> 'Nothing' <V> when call 'erlang':'is_map'(V) -> {'Just', V} <_> when 'true' -> 'Nothing' end"

-- | Get a string field from a JSON object
getString :: JsonValue -> String -> Maybe String
getString json field = getStringImpl json field

foreign import getStringImpl :: JsonValue -> String -> Maybe String
  = "case call 'erlang':'is_map'($0) of <'true'> when 'true' -> case call 'maps':'find'($1, $0) of <{'ok', V}> when call 'erlang':'is_binary'(V) -> {'Just', V} <_> when 'true' -> 'Nothing' end <_> when 'true' -> 'Nothing' end"

-- | Encode a value to JSON string
encode :: forall a. a -> String
encode val = encodeImpl val

foreign import encodeImpl :: forall a. a -> String
  = "call 'json':'encode'($0)"
