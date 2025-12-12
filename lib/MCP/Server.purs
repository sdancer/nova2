-- | MCP Server - Standalone server with NamespaceService integration
module MCP.Server where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Nova.NamespaceService as NS

-- | Server state (opaque - actually NS.ServiceState at runtime)
type ServerState = Unit

-- | Coerce any value to ServerState (same at runtime)
toServerState :: forall a. a -> ServerState
toServerState st = toServerStateImpl st

foreign import toServerStateImpl :: forall a. a -> ServerState
  = "$0"

-- | Coerce ServerState to any value (same at runtime)
fromServerState :: forall a. ServerState -> a
fromServerState st = fromServerStateImpl st

foreign import fromServerStateImpl :: forall a. ServerState -> a
  = "$0"

-- | Run the MCP server with STDIO transport
runStdio :: Unit -> Unit
runStdio _u =
  let _log = logInfo "Nova MCP Server starting (STDIO)"
  in case NS.init unit of
    Left err ->
      let _log2 = logInfo ("Failed to init NamespaceService: " <> err)
      in unit
    Right state ->
      let _log2 = logInfo "NamespaceService initialized"
      in serverLoop (toServerState state)

-- | Main server loop with state
serverLoop :: ServerState -> Unit
serverLoop state =
  case readLineImpl unit of
    Left err ->
      let _log = logInfo ("Server shutting down: " <> err)
          _w = NS.shutdown (fromServerState state)
      in unit
    Right line ->
      if line == ""
      then serverLoop state
      else
        let result = handleRequest state line
            _w = writeLineImpl result.response
        in serverLoop result.state

-- | Request result with potentially modified state
type RequestResult = { response :: String, state :: ServerState }

-- | Handle a JSON-RPC request
handleRequest :: ServerState -> String -> RequestResult
handleRequest state jsonStr =
  let method = extractMethod jsonStr
      reqId = extractId jsonStr
  in if method == "initialize"
     then { response: mkSuccessResponse reqId serverInfoJson, state: state }
     else if method == "notifications/initialized"
     then { response: "", state: state }
     else if method == "tools/list"
     then { response: mkSuccessResponse reqId toolsListJson, state: state }
     else if method == "tools/call"
     then handleToolCall state reqId jsonStr
     else if method == "ping"
     then { response: mkSuccessResponse reqId "{}", state: state }
     else { response: mkErrorResponse reqId ("Method not found: " <> method), state: state }

-- | Handle tool calls
handleToolCall :: ServerState -> String -> String -> RequestResult
handleToolCall state reqId jsonStr =
  let toolName = extractToolName jsonStr
  in if toolName == "create_namespace"
     then handleCreateNamespace state reqId jsonStr
     else if toolName == "list_namespaces"
     then handleListNamespaces state reqId
     else if toolName == "add_declaration"
     then handleAddDeclaration state reqId jsonStr
     else if toolName == "get_declaration"
     then handleGetDeclaration state reqId jsonStr
     else if toolName == "list_declarations"
     then handleListDeclarations state reqId jsonStr
     else { response: mkErrorResponse reqId ("Unknown tool: " <> toolName), state: state }

-- | Create namespace tool
handleCreateNamespace :: ServerState -> String -> String -> RequestResult
handleCreateNamespace state reqId jsonStr =
  let name = extractArg jsonStr "name"
  in case NS.createNamespace (fromServerState state) name of
    Right _u -> { response: mkToolSuccess reqId ("Created namespace: " <> name), state: state }
    Left err -> { response: mkToolError reqId err, state: state }

-- | List namespaces tool
handleListNamespaces :: ServerState -> String -> RequestResult
handleListNamespaces state reqId =
  let namespaces = NS.listNamespaces (fromServerState state)
      json = arrayToJson namespaces
  in { response: mkToolSuccess reqId json, state: state }

-- | Add declaration tool
handleAddDeclaration :: ServerState -> String -> String -> RequestResult
handleAddDeclaration state reqId jsonStr =
  let namespace = extractArg jsonStr "namespace"
      name = extractArg jsonStr "name"
      source = extractArg jsonStr "source"
  in case NS.addDecl (fromServerState state) namespace name source NS.FunctionDecl of
    Right declId -> { response: mkToolSuccess reqId ("Added declaration: " <> declId), state: state }
    Left err -> { response: mkToolError reqId err, state: state }

-- | Get declaration tool
handleGetDeclaration :: ServerState -> String -> String -> RequestResult
handleGetDeclaration state reqId jsonStr =
  let declId = extractArg jsonStr "decl_id"
  in case NS.getDecl (fromServerState state) declId of
    Just decl -> { response: mkToolSuccess reqId (declToJson decl), state: state }
    Nothing -> { response: mkToolError reqId "Declaration not found", state: state }

-- | List declarations in namespace
handleListDeclarations :: ServerState -> String -> String -> RequestResult
handleListDeclarations state reqId jsonStr =
  let namespace = extractArg jsonStr "namespace"
      decls = NS.getNamespaceDecls (fromServerState state) namespace
      json = declsToJson decls
  in { response: mkToolSuccess reqId json, state: state }

-- | Convert declaration to JSON
declToJson :: NS.ManagedDecl -> String
declToJson decl =
  "{\"declId\":\"" <> decl.declId <> "\",\"name\":\"" <> decl.name <> "\",\"namespace\":\"" <> decl.namespace <> "\",\"source\":\"" <> escapeJson decl.sourceText <> "\"}"

-- | Convert declarations array to JSON
declsToJson :: Array NS.ManagedDecl -> String
declsToJson decls = "[" <> joinDecls decls <> "]"

joinDecls :: Array NS.ManagedDecl -> String
joinDecls decls = joinDeclsImpl decls

foreign import joinDeclsImpl :: Array NS.ManagedDecl -> String
  = "let <Jsons> = call 'lists':'map'(fun (D) -> let <Id> = call 'maps':'get'('declId', D) in let <Name> = call 'maps':'get'('name', D) in let <Ns> = call 'maps':'get'('namespace', D) in call 'erlang':'++'([123,34,100,101,99,108,73,100,34,58,34], call 'erlang':'++'(Id, call 'erlang':'++'([34,44,34,110,97,109,101,34,58,34], call 'erlang':'++'(Name, call 'erlang':'++'([34,44,34,110,97,109,101,115,112,97,99,101,34,58,34], call 'erlang':'++'(Ns, [34,125])))))), $0) in call 'string':'join'(Jsons, [44])"

-- | Convert string array to JSON
arrayToJson :: Array String -> String
arrayToJson arr = arrayToJsonImpl arr

foreign import arrayToJsonImpl :: Array String -> String
  = "let <Quoted> = call 'lists':'map'(fun (S) -> call 'erlang':'++'([34], call 'erlang':'++'(S, [34])), $0) in call 'erlang':'++'([91], call 'erlang':'++'(call 'string':'join'(Quoted, [44]), [93]))"

-- | Escape JSON string
escapeJson :: String -> String
escapeJson s = escapeJsonImpl s

foreign import escapeJsonImpl :: String -> String
  = "call 'lists':'flatten'(call 'lists':'map'(fun (C) -> case C of <34> when 'true' -> [92,34] <92> when 'true' -> [92,92] <10> when 'true' -> [92,110] <_> when 'true' -> [C] end, $0))"

-- | Make tool success response
mkToolSuccess :: String -> String -> String
mkToolSuccess reqId content =
  "{\"jsonrpc\":\"2.0\",\"id\":" <> reqId <> ",\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"" <> escapeJson content <> "\"}]}}"

-- | Make tool error response
mkToolError :: String -> String -> String
mkToolError reqId msg =
  "{\"jsonrpc\":\"2.0\",\"id\":" <> reqId <> ",\"result\":{\"isError\":true,\"content\":[{\"type\":\"text\",\"text\":\"" <> escapeJson msg <> "\"}]}}"

-- | Server info JSON
serverInfoJson :: String
serverInfoJson = "{\"protocolVersion\":\"2024-11-05\",\"serverInfo\":{\"name\":\"nova-compiler\",\"version\":\"0.1.0\"},\"capabilities\":{\"tools\":{}}}"

-- | Tools list JSON with namespace tools
toolsListJson :: String
toolsListJson = "{\"tools\":[{\"name\":\"create_namespace\",\"description\":\"Create a new namespace for declarations\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\",\"description\":\"Name of the namespace\"}},\"required\":[\"name\"]}},{\"name\":\"list_namespaces\",\"description\":\"List all namespaces\",\"inputSchema\":{\"type\":\"object\",\"properties\":{}}},{\"name\":\"add_declaration\",\"description\":\"Add a declaration to a namespace\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"namespace\":{\"type\":\"string\",\"description\":\"Target namespace\"},\"name\":{\"type\":\"string\",\"description\":\"Declaration name\"},\"source\":{\"type\":\"string\",\"description\":\"Source code\"}},\"required\":[\"namespace\",\"name\",\"source\"]}},{\"name\":\"get_declaration\",\"description\":\"Get a declaration by ID\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"decl_id\":{\"type\":\"string\",\"description\":\"Declaration ID\"}},\"required\":[\"decl_id\"]}},{\"name\":\"list_declarations\",\"description\":\"List declarations in a namespace\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"namespace\":{\"type\":\"string\",\"description\":\"Namespace to list\"}},\"required\":[\"namespace\"]}}]}"

-- | Make success response
mkSuccessResponse :: String -> String -> String
mkSuccessResponse reqId result =
  "{\"jsonrpc\":\"2.0\",\"id\":" <> reqId <> ",\"result\":" <> result <> "}"

-- | Make error response
mkErrorResponse :: String -> String -> String
mkErrorResponse reqId msg =
  "{\"jsonrpc\":\"2.0\",\"id\":" <> reqId <> ",\"error\":{\"code\":-32601,\"message\":\"" <> msg <> "\"}}"

-- | Extract method from JSON request
extractMethod :: String -> String
extractMethod s = extractMethodImpl s

foreign import extractMethodImpl :: String -> String
  = "let <Map> = call 'json':'decode'(call 'erlang':'list_to_binary'($0)) in let <Key> = call 'erlang':'list_to_binary'([109,101,116,104,111,100]) in case call 'maps':'find'(Key, Map) of <{'ok', V}> when call 'erlang':'is_binary'(V) -> call 'erlang':'binary_to_list'(V) <_> when 'true' -> [] end"

-- | Extract id from JSON request
extractId :: String -> String
extractId s = extractIdImpl s

foreign import extractIdImpl :: String -> String
  = "let <Map> = call 'json':'decode'(call 'erlang':'list_to_binary'($0)) in let <Key> = call 'erlang':'list_to_binary'([105,100]) in case call 'maps':'find'(Key, Map) of <{'ok', V}> when call 'erlang':'is_integer'(V) -> call 'erlang':'integer_to_list'(V) <{'ok', V}> when call 'erlang':'is_binary'(V) -> call 'erlang':'++'([34], call 'erlang':'++'(call 'erlang':'binary_to_list'(V), [34])) <_> when 'true' -> [110,117,108,108] end"

-- | Extract tool name from tools/call request
extractToolName :: String -> String
extractToolName s = extractToolNameImpl s

foreign import extractToolNameImpl :: String -> String
  = "let <Map> = call 'json':'decode'(call 'erlang':'list_to_binary'($0)) in let <ParamsKey> = call 'erlang':'list_to_binary'([112,97,114,97,109,115]) in let <NameKey> = call 'erlang':'list_to_binary'([110,97,109,101]) in case call 'maps':'find'(ParamsKey, Map) of <{'ok', Params}> when call 'erlang':'is_map'(Params) -> case call 'maps':'find'(NameKey, Params) of <{'ok', V}> when call 'erlang':'is_binary'(V) -> call 'erlang':'binary_to_list'(V) <_> when 'true' -> [] end <_> when 'true' -> [] end"

-- | Extract argument from tools/call request
extractArg :: String -> String -> String
extractArg jsonStr argName = extractArgImpl jsonStr argName

foreign import extractArgImpl :: String -> String -> String
  = "let <Map> = call 'json':'decode'(call 'erlang':'list_to_binary'($0)) in let <ParamsKey> = call 'erlang':'list_to_binary'([112,97,114,97,109,115]) in let <ArgsKey> = call 'erlang':'list_to_binary'([97,114,103,117,109,101,110,116,115]) in let <ArgKey> = call 'erlang':'list_to_binary'($1) in case call 'maps':'find'(ParamsKey, Map) of <{'ok', Params}> when call 'erlang':'is_map'(Params) -> case call 'maps':'find'(ArgsKey, Params) of <{'ok', Args}> when call 'erlang':'is_map'(Args) -> case call 'maps':'find'(ArgKey, Args) of <{'ok', V}> when call 'erlang':'is_binary'(V) -> call 'erlang':'binary_to_list'(V) <_> when 'true' -> [] end <_> when 'true' -> [] end <_> when 'true' -> [] end"

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
