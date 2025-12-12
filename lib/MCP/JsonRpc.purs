-- | JSON-RPC 2.0 types and handling for MCP
module MCP.JsonRpc where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import MCP.Json (JsonValue, decode, encode, getField, isNull, isString, isNumber,
                 jsonStringToString, jsonNumberToInt, jsonNull, object, kv,
                 toJsonString, toJsonInt)

-- | JSON-RPC request ID (can be string, number, or null)
data RequestId
  = IdString String
  | IdInt Int
  | IdNull

-- | JSON-RPC request
type Request =
  { id :: Maybe RequestId
  , method :: String
  , params :: JsonValue
  }

-- | Standard JSON-RPC error codes
parseError :: Int
parseError = -32700

invalidRequest :: Int
invalidRequest = -32600

methodNotFound :: Int
methodNotFound = -32601

invalidParams :: Int
invalidParams = -32602

internalError :: Int
internalError = -32603

-- | Parse a JSON-RPC request from a JSON string
parseRequest :: String -> Either String Request
parseRequest jsonStr =
  case decode jsonStr of
    Left err -> Left ("JSON parse error: " <> err)
    Right json ->
      case getField "method" json of
        Nothing -> Left "Missing 'method' field"
        Just methodVal ->
          if isString methodVal
          then
            let method = jsonStringToString methodVal
                params = case getField "params" json of
                  Nothing -> object []
                  Just p -> p
                reqId = parseId json
            in Right { id: reqId, method: method, params: params }
          else Left "'method' must be a string"

-- | Parse the request ID from a JSON object
parseId :: JsonValue -> Maybe RequestId
parseId json =
  case getField "id" json of
    Nothing -> Nothing
    Just idVal ->
      if isNull idVal then Just IdNull
      else if isString idVal then Just (IdString (jsonStringToString idVal))
      else if isNumber idVal then Just (IdInt (jsonNumberToInt idVal))
      else Nothing

-- | Check if request is a notification (no id)
isNotification :: Request -> Boolean
isNotification req = case req.id of
  Nothing -> true
  Just IdNull -> false
  _ -> false

-- | Encode a success response to JSON string
encodeSuccess :: RequestId -> JsonValue -> String
encodeSuccess reqId result =
  let idJson = encodeId reqId
      response = object
        [ kv "jsonrpc" (toJsonString "2.0")
        , kv "id" idJson
        , kv "result" result
        ]
  in encode response

-- | Encode an error response to JSON string
encodeError :: Maybe RequestId -> Int -> String -> Maybe JsonValue -> String
encodeError maybeId code message maybeData =
  let idJson = case maybeId of
        Nothing -> jsonNull
        Just reqId -> encodeId reqId
      errorObj = case maybeData of
        Nothing -> object
          [ kv "code" (toJsonInt code)
          , kv "message" (toJsonString message)
          ]
        Just d -> object
          [ kv "code" (toJsonInt code)
          , kv "message" (toJsonString message)
          , kv "data" d
          ]
      response = object
        [ kv "jsonrpc" (toJsonString "2.0")
        , kv "id" idJson
        , kv "error" errorObj
        ]
  in encode response

-- | Encode a request ID to JSON
encodeId :: RequestId -> JsonValue
encodeId IdNull = jsonNull
encodeId (IdString s) = toJsonString s
encodeId (IdInt n) = toJsonInt n

-- | Create a parse error response
mkParseError :: String -> String
mkParseError msg = encodeError Nothing parseError msg Nothing

-- | Create a method not found error response
mkMethodNotFound :: RequestId -> String -> String
mkMethodNotFound reqId method =
  encodeError (Just reqId) methodNotFound ("Method not found: " <> method) Nothing

-- | Create an internal error response
mkInternalError :: RequestId -> String -> String
mkInternalError reqId msg =
  encodeError (Just reqId) internalError msg Nothing
