-- | MCP Tool definitions and handlers
module MCP.Tools where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Map as Map
import Data.String as String
import MCP.Json (JsonValue, object, kv, toJsonString, toJsonBool, toJsonArray,
                 getField, isString, jsonStringToString)

-- Inline types
type ServerState =
  { initialized :: Boolean
  , namespaces :: Map.Map String Namespace
  , nextDeclId :: Int
  }

type Namespace =
  { name :: String
  , declarations :: Map.Map String ManagedDecl
  , imports :: Array String
  }

type ManagedDecl =
  { meta :: { declId :: String, name :: String, kind :: String, status :: String, version :: Int }
  , sourceText :: String
  }

-- | Tool definition schema
type ToolDef =
  { name :: String
  , description :: String
  , inputSchema :: JsonValue
  }

-- | List of all available tools as JSON
listToolsJson :: Unit -> JsonValue
listToolsJson _ =
  object [ kv "tools" (toJsonArray (map toolToJson allTools)) ]

-- | Convert tool definition to JSON
toolToJson :: ToolDef -> JsonValue
toolToJson tool =
  object
    [ kv "name" (toJsonString tool.name)
    , kv "description" (toJsonString tool.description)
    , kv "inputSchema" tool.inputSchema
    ]

-- | All available tools
allTools :: Array ToolDef
allTools =
  [ { name: "nova_create_namespace"
    , description: "Create a new namespace for organizing Nova declarations"
    , inputSchema: objectSchema [ prop "name" "string" "The namespace name" ] ["name"]
    }
  , { name: "nova_delete_namespace"
    , description: "Delete a namespace and all its declarations"
    , inputSchema: objectSchema [ prop "name" "string" "The namespace name to delete" ] ["name"]
    }
  , { name: "nova_list_namespaces"
    , description: "List all existing namespaces"
    , inputSchema: objectSchema [] []
    }
  , { name: "nova_echo"
    , description: "Echo back the provided message (for testing)"
    , inputSchema: objectSchema [ prop "message" "string" "Message to echo" ] ["message"]
    }
  ]

-- | Helper to create object schema
objectSchema :: Array { name :: String, ty :: String, desc :: String } -> Array String -> JsonValue
objectSchema props required =
  let propsObj = object (map propToKv props)
      requiredArr = toJsonArray (map toJsonString required)
  in object
    [ kv "type" (toJsonString "object")
    , kv "properties" propsObj
    , kv "required" requiredArr
    ]

propToKv :: { name :: String, ty :: String, desc :: String } -> { key :: String, value :: JsonValue }
propToKv p =
  kv p.name (object
    [ kv "type" (toJsonString p.ty)
    , kv "description" (toJsonString p.desc)
    ])

prop :: String -> String -> String -> { name :: String, ty :: String, desc :: String }
prop name ty desc = { name: name, ty: ty, desc: desc }

-- | Call a tool by name with arguments
callTool :: String -> JsonValue -> ServerState -> { result :: JsonValue, state :: ServerState }
callTool name args state =
  let result = doCallTool name args state
  in { result: formatResult result.result, state: result.state }

-- | Format a tool result
formatResult :: Either String String -> JsonValue
formatResult (Right text) =
  object
    [ kv "content" (toJsonArray [ object [ kv "type" (toJsonString "text"), kv "text" (toJsonString text) ] ])
    , kv "isError" (toJsonBool false)
    ]
formatResult (Left err) =
  object
    [ kv "content" (toJsonArray [ object [ kv "type" (toJsonString "text"), kv "text" (toJsonString err) ] ])
    , kv "isError" (toJsonBool true)
    ]

-- | Internal tool dispatch
doCallTool :: String -> JsonValue -> ServerState -> { result :: Either String String, state :: ServerState }
doCallTool "nova_create_namespace" args state =
  let name = getStringArg "name" args
      ns = { name: name, declarations: Map.empty, imports: [] }
  in case Map.lookup name state.namespaces of
    Just _ -> { result: Left ("Namespace '" <> name <> "' already exists"), state: state }
    Nothing ->
      let newState = state { namespaces = Map.insert name ns state.namespaces }
      in { result: Right ("Created namespace: " <> name), state: newState }

doCallTool "nova_delete_namespace" args state =
  let name = getStringArg "name" args
  in case Map.lookup name state.namespaces of
    Nothing -> { result: Left ("Namespace '" <> name <> "' not found"), state: state }
    Just _ ->
      let newState = state { namespaces = Map.delete name state.namespaces }
      in { result: Right ("Deleted namespace: " <> name), state: newState }

doCallTool "nova_list_namespaces" _ state =
  let names = Array.fromFoldable (Map.keys state.namespaces)
  in if Array.length names == 0
     then { result: Right "No namespaces exist yet.", state: state }
     else { result: Right ("Namespaces:\n" <> String.joinWith "\n" names), state: state }

doCallTool "nova_echo" args state =
  let msg = getStringArg "message" args
  in { result: Right ("Echo: " <> msg), state: state }

doCallTool name _ state =
  { result: Left ("Unknown tool: " <> name), state: state }

-- | Helper to get a string argument
getStringArg :: String -> JsonValue -> String
getStringArg key obj =
  case getField key obj of
    Nothing -> ""
    Just v ->
      if isString v
      then jsonStringToString v
      else ""
