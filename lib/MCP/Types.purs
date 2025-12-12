-- | Shared types for MCP modules
module MCP.Types where

import Prelude
import Data.Array (Array)
import Data.Map (Map)
import Data.Map as Map

-- | Declaration metadata
type DeclMeta =
  { declId :: String
  , name :: String
  , kind :: String      -- "function", "data", "type", "class", "instance"
  , status :: String    -- "pending", "valid", "invalid"
  , version :: Int
  }

-- | Managed declaration with source and parsed AST
type ManagedDecl =
  { meta :: DeclMeta
  , sourceText :: String
  , parsedDecl :: forall a. a  -- Opaque AST
  , inferredType :: forall a. a  -- Opaque type
  }

-- | Namespace state
type Namespace =
  { name :: String
  , declarations :: Map String ManagedDecl
  , imports :: Array String
  }

-- | Server state
type ServerState =
  { initialized :: Boolean
  , namespaces :: Map String Namespace
  , nextDeclId :: Int
  }

-- | Initial server state
initialState :: ServerState
initialState =
  { initialized: false
  , namespaces: Map.empty
  , nextDeclId: 1
  }

-- | Generate a new declaration ID
genDeclId :: ServerState -> { id :: String, state :: ServerState }
genDeclId state =
  let id = "decl_" <> show state.nextDeclId
  in { id: id, state: state { nextDeclId = state.nextDeclId + 1 } }

-- | Create an empty namespace
emptyNamespace :: String -> Namespace
emptyNamespace name =
  { name: name
  , declarations: Map.empty
  , imports: []
  }
