-- | JSON encoding/decoding using OTP 27+ built-in json module
module MCP.Json where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | Opaque JSON value type (Erlang term)
type JsonValue = Unit

-- | Encode any Erlang term to JSON string
encode :: forall a. a -> String
encode x = encodeImpl x

foreign import encodeImpl :: forall a. a -> String
  = "call 'erlang':'binary_to_list'(call 'erlang':'iolist_to_binary'(call 'json':'encode'($0)))"

-- | Decode JSON string to Erlang term
decode :: String -> Either String JsonValue
decode s = decodeImpl s

foreign import decodeImpl :: String -> Either String JsonValue
  = "case catch call 'json':'decode'(call 'erlang':'list_to_binary'($0)) of <{'EXIT', _R}> when 'true' -> {'Left', []} <Result> when 'true' -> {'Right', Result} end"

-- | Check if value is null
isNull :: JsonValue -> Boolean
isNull v = isNullImpl v

foreign import isNullImpl :: JsonValue -> Boolean
  = "case $0 of <'null'> when 'true' -> 'true' <_> when 'true' -> 'false' end"

-- | Check if value is a string (binary in Erlang)
isString :: JsonValue -> Boolean
isString v = isStringImpl v

foreign import isStringImpl :: JsonValue -> Boolean
  = "call 'erlang':'is_binary'($0)"

-- | Check if value is a number
isNumber :: JsonValue -> Boolean
isNumber v = isNumberImpl v

foreign import isNumberImpl :: JsonValue -> Boolean
  = "call 'erlang':'is_number'($0)"

-- | Check if value is a map
isMap :: JsonValue -> Boolean
isMap v = isMapImpl v

foreign import isMapImpl :: JsonValue -> Boolean
  = "call 'erlang':'is_map'($0)"

-- | Get a field from a JSON object
getField :: String -> JsonValue -> Maybe JsonValue
getField k obj = getFieldImpl k obj

foreign import getFieldImpl :: String -> JsonValue -> Maybe JsonValue
  = "case call 'maps':'find'(call 'erlang':'list_to_binary'($0), $1) of <{'ok', Val}> when 'true' -> {'Just', Val} <'error'> when 'true' -> 'Nothing' end"

-- | Convert JSON string (binary) to Erlang string (charlist)
jsonStringToString :: JsonValue -> String
jsonStringToString v = jsonStringToStringImpl v

foreign import jsonStringToStringImpl :: JsonValue -> String
  = "call 'erlang':'binary_to_list'($0)"

-- | Convert JSON number to Int
jsonNumberToInt :: JsonValue -> Int
jsonNumberToInt v = jsonNumberToIntImpl v

foreign import jsonNumberToIntImpl :: JsonValue -> Int
  = "call 'erlang':'trunc'($0)"

-- | Build JSON object from key-value pairs
object :: Array { key :: String, value :: JsonValue } -> JsonValue
object pairs = objectImpl pairs

foreign import objectImpl :: Array { key :: String, value :: JsonValue } -> JsonValue
  = "let <List> = call 'erlang':'tl'(call 'erlang':'tuple_to_list'($0)) in let <F> = fun (R) -> let <K> = call 'erlang':'list_to_binary'(call 'maps':'get'('key',R)) in let <V> = call 'maps':'get'('value',R) in {K,V} in call 'maps':'from_list'(call 'lists':'map'(F, List))"

-- | Convert String to JsonValue (binary)
toJsonString :: String -> JsonValue
toJsonString s = toJsonStringImpl s

foreign import toJsonStringImpl :: String -> JsonValue
  = "call 'erlang':'list_to_binary'($0)"

-- | Convert Int to JsonValue
toJsonInt :: Int -> JsonValue
toJsonInt n = toJsonIntImpl n

foreign import toJsonIntImpl :: Int -> JsonValue
  = "$0"

-- | Convert Boolean to JsonValue
toJsonBool :: Boolean -> JsonValue
toJsonBool b = toJsonBoolImpl b

foreign import toJsonBoolImpl :: Boolean -> JsonValue
  = "$0"

-- | JSON null value
jsonNull :: JsonValue
jsonNull = jsonNullImpl

foreign import jsonNullImpl :: JsonValue
  = "'null'"

-- | Convert Array to JSON array (list)
toJsonArray :: Array JsonValue -> JsonValue
toJsonArray arr = toJsonArrayImpl arr

foreign import toJsonArrayImpl :: Array JsonValue -> JsonValue
  = "call 'erlang':'tl'(call 'erlang':'tuple_to_list'($0))"

-- Helper to create a key-value pair
kv :: String -> JsonValue -> { key :: String, value :: JsonValue }
kv k v = { key: k, value: v }
