-- | MCP Server - Main server loop and request handling
module MCP.Server where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Map as Map
import MCP.Json (JsonValue, object, kv, toJsonString, jsonNull)
import MCP.JsonRpc (Request, RequestId, parseRequest, mkParseError, mkInternalError, encodeSuccess)
import MCP.Transport (Transport, initStdio, initTcp, readLine, writeLine, closeTransport, logInfo, logError)
import MCP.Tools (listToolsJson, callTool, ServerState)

-- | Initial server state
initialState :: ServerState
initialState =
  { initialized: false
  , namespaces: Map.empty
  , nextDeclId: 1
  }

-- | Server info returned in initialize response
serverInfo :: JsonValue
serverInfo = object
  [ kv "protocolVersion" (toJsonString "2024-11-05")
  , kv "serverInfo" (object
      [ kv "name" (toJsonString "nova-compiler")
      , kv "version" (toJsonString "0.1.0")
      ])
  , kv "capabilities" (object
      [ kv "tools" (object [])
      ])
  ]

-- | Run the MCP server with STDIO transport
runStdio :: Unit -> Unit
runStdio _ =
  let transport = initStdio
      _ = logInfo "Nova MCP Server starting (STDIO)"
  in serverLoop transport initialState

-- | Run the MCP server with TCP transport
runTcp :: Int -> Either String Unit
runTcp port =
  case initTcp port of
    Left err -> Left err
    Right transport -> do
      let _ = logInfo "Nova MCP Server starting (TCP)"
      Right (serverLoop transport initialState)

-- | Main server loop
serverLoop :: Transport -> ServerState -> Unit
serverLoop transport state =
  case readLine transport of
    Left err -> do
      let _ = logInfo ("Server shutting down: " <> err)
      closeTransport transport
    Right line ->
      if line == ""
      then serverLoop transport state
      else do
        let result = handleRequest line state
        let newState = result.state
        case result.response of
          Nothing -> serverLoop transport newState
          Just response -> do
            case writeLine transport response of
              Left err -> do
                let _ = logError ("Failed to send response: " <> err)
                closeTransport transport
              Right _ -> serverLoop transport newState

-- | Handle a JSON-RPC request
handleRequest :: String -> ServerState -> { response :: Maybe String, state :: ServerState }
handleRequest jsonStr state =
  case parseRequest jsonStr of
    Left err ->
      { response: Just (mkParseError err), state: state }
    Right req ->
      let result = dispatchMethod req state
      in case req.id of
        Nothing -> { response: Nothing, state: result.state }
        Just reqId ->
          case result.response of
            Left errMsg ->
              { response: Just (mkInternalError reqId errMsg)
              , state: result.state
              }
            Right jsonResult ->
              { response: Just (encodeSuccess reqId jsonResult)
              , state: result.state
              }

-- | Dispatch to the appropriate method handler
dispatchMethod :: Request -> ServerState -> { response :: Either String JsonValue, state :: ServerState }
dispatchMethod req state =
  case req.method of
    "initialize" ->
      { response: Right serverInfo
      , state: state { initialized = true }
      }

    "notifications/initialized" ->
      { response: Right jsonNull
      , state: state
      }

    "tools/list" ->
      { response: Right (listToolsJson unit)
      , state: state
      }

    "tools/call" ->
      let toolResult = handleToolCall req.params state
      in { response: Right toolResult.result
         , state: toolResult.state
         }

    "ping" ->
      { response: Right (object [])
      , state: state
      }

    _ ->
      case req.id of
        Just _ ->
          { response: Left ("Method not found: " <> req.method)
          , state: state
          }
        Nothing ->
          { response: Right jsonNull
          , state: state
          }

-- | Handle a tools/call request
handleToolCall :: JsonValue -> ServerState -> { result :: JsonValue, state :: ServerState }
handleToolCall params state =
  let toolName = getToolName params
      arguments = getArguments params
  in callTool toolName arguments state

-- | Extract tool name from params
foreign import getToolName :: JsonValue -> String
  = "case call 'maps':'find'(<<\"name\">>, $0) of <{'ok', Name}> when call 'erlang':'is_binary'(Name) -> call 'erlang':'binary_to_list'(Name) <_> when 'true' -> [] end"

-- | Extract arguments from params
foreign import getArguments :: JsonValue -> JsonValue
  = "case call 'maps':'find'(<<\"arguments\">>, $0) of <{'ok', Args}> when 'true' -> Args <'error'> when 'true' -> ~{}~ end"
