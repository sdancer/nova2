-- | MCP Server - Standalone server with inline JSON handling
module MCP.Server where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | Run the MCP server with STDIO transport
runStdio :: Unit -> Unit
runStdio _u =
  let _log = logInfo "Nova MCP Server starting (STDIO)"
  in serverLoop unit

-- | Main server loop
serverLoop :: Unit -> Unit
serverLoop _u =
  case readLineImpl unit of
    Left err ->
      let _log = logInfo ("Server shutting down: " <> err)
      in unit
    Right line ->
      if line == ""
      then serverLoop unit
      else
        let response = handleRequest line
            _w = writeLineImpl response
        in serverLoop unit

-- | Handle a JSON-RPC request
handleRequest :: String -> String
handleRequest jsonStr =
  let method = extractMethod jsonStr
      reqId = extractId jsonStr
  in if method == "initialize"
     then mkSuccessResponse reqId serverInfoJson
     else if method == "notifications/initialized"
     then ""  -- No response for notifications
     else if method == "tools/list"
     then mkSuccessResponse reqId toolsListJson
     else if method == "ping"
     then mkSuccessResponse reqId "{}"
     else mkErrorResponse reqId ("Method not found: " <> method)

-- | Server info JSON
serverInfoJson :: String
serverInfoJson = "{\"protocolVersion\":\"2024-11-05\",\"serverInfo\":{\"name\":\"nova-compiler\",\"version\":\"0.1.0\"},\"capabilities\":{\"tools\":{}}}"

-- | Tools list JSON
toolsListJson :: String
toolsListJson = "{\"tools\":[{\"name\":\"nova_echo\",\"description\":\"Echo back the provided message\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"message\":{\"type\":\"string\",\"description\":\"Message to echo\"}},\"required\":[\"message\"]}}]}"

-- | Make success response
mkSuccessResponse :: String -> String -> String
mkSuccessResponse reqId result =
  "{\"jsonrpc\":\"2.0\",\"id\":" <> reqId <> ",\"result\":" <> result <> "}"

-- | Make error response
mkErrorResponse :: String -> String -> String
mkErrorResponse reqId msg =
  "{\"jsonrpc\":\"2.0\",\"id\":" <> reqId <> ",\"error\":{\"code\":-32601,\"message\":\"" <> msg <> "\"}}"

-- | Extract method from JSON request using json module
extractMethod :: String -> String
extractMethod s = extractMethodImpl s

foreign import extractMethodImpl :: String -> String
  = "let <Map> = call 'json':'decode'(call 'erlang':'list_to_binary'($0)) in let <Key> = call 'erlang':'list_to_binary'([109,101,116,104,111,100]) in case call 'maps':'find'(Key, Map) of <{'ok', V}> when call 'erlang':'is_binary'(V) -> call 'erlang':'binary_to_list'(V) <_> when 'true' -> [] end"

-- | Extract id from JSON request
extractId :: String -> String
extractId s = extractIdImpl s

foreign import extractIdImpl :: String -> String
  = "let <Map> = call 'json':'decode'(call 'erlang':'list_to_binary'($0)) in let <Key> = call 'erlang':'list_to_binary'([105,100]) in case call 'maps':'find'(Key, Map) of <{'ok', V}> when call 'erlang':'is_integer'(V) -> call 'erlang':'integer_to_list'(V) <{'ok', V}> when call 'erlang':'is_binary'(V) -> call 'erlang':'++'([34], call 'erlang':'++'(call 'erlang':'binary_to_list'(V), [34])) <_> when 'true' -> [110,117,108,108] end"

-- | Read line from STDIO (converts binary to list)
foreign import readLineImpl :: Unit -> Either String String
  = "let <Line0> = call 'io':'get_line'('') in case Line0 of <'eof'> when 'true' -> {'Left', [101,111,102]} <{'error', _R}> when 'true' -> {'Left', [101,114,114]} <_> when 'true' -> let <Trimmed> = call 'string':'trim'(Line0) in let <AsList> = case call 'erlang':'is_binary'(Trimmed) of <'true'> when 'true' -> call 'erlang':'binary_to_list'(Trimmed) <'false'> when 'true' -> Trimmed end in {'Right', AsList} end"

-- | Write line to STDOUT
foreign import writeLineImpl :: String -> Unit
  = "let <_> = call 'io':'put_chars'($0) in let <_> = call 'io':'nl'() in 'unit'"

-- | Log to stderr
logInfo :: String -> Unit
logInfo msg = logInfoImpl msg

foreign import logInfoImpl :: String -> Unit
  = "let <_> = call 'io':'put_chars'('standard_error', [[91,73,78,70,79,93,32], $0]) in let <_> = call 'io':'nl'('standard_error') in 'unit'"
