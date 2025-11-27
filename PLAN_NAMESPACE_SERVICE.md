# Nova Namespace Service Architecture Plan

## Vision

Transform the Nova compiler from a file-based batch compiler into an LSP-like namespace service where:
- Code elements (declarations) can be **added/edited/queried/removed** dynamically
- **Namespaces** replace files as the organizational unit
- **Incremental compilation** - only re-check what changed
- **Query API** - get types, completions, diagnostics on demand

## Core Concepts

### Namespace vs Module
```
Current (file-based):
  file.purs → Module { name, declarations[] }

New (namespace-based):
  Namespace "MyApp.Core" → {
    declarations: Map<DeclId, Declaration>
    dependencies: Graph<DeclId>
    types: Map<DeclId, Scheme>
  }
```

### Declaration Identity
Every declaration gets a unique ID:
```
DeclId = "namespace:kind:name:version"
Examples:
  "MyApp.Core:function:add:1"
  "MyApp.Core:datatype:Color:1"
  "MyApp.Core:typealias:UserId:1"
```

### Operations
```
add(namespace, declaration) → DeclId
update(declId, newDeclaration) → DeclId
remove(declId) → ()
query(declId, what) → Result
  - type: Get inferred type
  - diagnostics: Get errors/warnings
  - references: Where is this used?
  - definition: Where is this defined?
  - completions: What's available here?
```

---

## Phase 1: Foundation (Declaration Metadata)

### 1.1 Add Declaration Wrapper

**File: `src/Nova/Compiler/Ast.purs`**

```purescript
-- New types
type DeclId = String

data DeclStatus
  = Fresh        -- Not yet type-checked
  | Valid        -- Type-checked OK
  | Invalid      -- Has type errors
  | Stale        -- Needs re-check (dependency changed)

type DeclMetadata =
  { declId :: DeclId
  , namespace :: String
  , version :: Int
  , status :: DeclStatus
  , dependencies :: Set DeclId    -- What this depends on
  , dependents :: Set DeclId      -- What depends on this
  }

type ManagedDecl =
  { meta :: DeclMetadata
  , decl :: Declaration
  , inferredType :: Maybe Scheme  -- Cached after type-check
  , errors :: Array TCError       -- Cached errors
  }
```

### 1.2 Build Dependency Graph

**File: `src/Nova/Compiler/Dependencies.purs`** (new)

```purescript
module Nova.Compiler.Dependencies where

-- Extract what names a declaration references
getDependencies :: Declaration -> Set String

-- Given a namespace, build full dependency graph
buildDependencyGraph :: Map DeclId ManagedDecl -> Graph DeclId

-- Get transitive dependents (what to invalidate)
getAffected :: Graph DeclId -> DeclId -> Set DeclId
```

### 1.3 Elixir Service Skeleton

**File: `nova_lang/lib/nova/namespace_service.ex`** (new)

```elixir
defmodule Nova.NamespaceService do
  use GenServer

  defstruct [
    namespaces: %{},      # namespace_id -> NamespaceState
    global_counter: 0     # For generating unique IDs
  ]

  defmodule NamespaceState do
    defstruct [
      declarations: %{},  # decl_id -> ManagedDecl
      dep_graph: nil,     # Dependency graph
      type_env: nil       # Current Env snapshot
    ]
  end

  # Public API
  def start_link(opts \\ [])
  def create_namespace(pid, name)
  def add_declaration(pid, namespace, source)
  def update_declaration(pid, decl_id, source)
  def remove_declaration(pid, decl_id)
  def get_type(pid, decl_id)
  def get_diagnostics(pid, namespace)
  def get_completions(pid, namespace, prefix)
end
```

---

## Phase 2: Incremental Type Checking

### 2.1 Single-Declaration Type Checker

**File: `src/Nova/Compiler/TypeChecker.purs`** (modify)

```purescript
-- New: Check single declaration with existing environment
checkSingleDecl :: Env -> ManagedDecl -> Either TCError CheckResult

type CheckResult =
  { decl :: ManagedDecl       -- Updated with type info
  , env :: Env                -- Updated environment
  , newBindings :: Map String Scheme  -- What this added
  }

-- New: Validate only dirty declarations
validateDirty :: Env -> Array ManagedDecl -> Either TCError (Array ManagedDecl)
```

### 2.2 Caching Layer

**File: `src/Nova/Compiler/Cache.purs`** (new)

```purescript
module Nova.Compiler.Cache where

type TypeCache =
  { declId :: DeclId
  , version :: Int
  , result :: Either TCError Scheme
  }

-- Check if cache is valid for declaration
isCacheValid :: TypeCache -> ManagedDecl -> Boolean

-- Invalidate caches for affected declarations
invalidateCaches :: Set DeclId -> Map DeclId TypeCache -> Map DeclId TypeCache
```

### 2.3 Incremental Validation Pipeline

```
On add_declaration(namespace, source):
  1. Parse source → Declaration
  2. Generate DeclId
  3. Extract dependencies → Set String
  4. Create ManagedDecl with status=Fresh
  5. Add to namespace.declarations
  6. Update dependency graph
  7. Return DeclId

On update_declaration(declId, source):
  1. Parse source → Declaration
  2. Get old ManagedDecl
  3. Compute affected = getAffected(graph, declId)
  4. Mark affected as Stale
  5. Update declaration, bump version
  6. Rebuild dependency edges
  7. Trigger validation for declId

On get_diagnostics(namespace):
  1. Find all Fresh/Stale declarations
  2. Topological sort by dependencies
  3. Validate in order, caching results
  4. Collect all errors
  5. Return diagnostics with locations
```

---

## Phase 3: Query API

### 3.1 Type Queries

```elixir
defmodule Nova.NamespaceService.Queries do
  # Get type of a specific declaration
  def get_type(state, decl_id)

  # Get type at a position in declaration source
  def get_type_at(state, decl_id, line, column)

  # Get all available names at a point
  def get_scope(state, namespace, context)

  # Get completions for prefix
  def get_completions(state, namespace, prefix, context)
end
```

### 3.2 Navigation Queries

```elixir
# Go to definition
def get_definition(state, namespace, name)

# Find all references
def get_references(state, decl_id)

# Get call hierarchy
def get_callers(state, decl_id)
def get_callees(state, decl_id)
```

### 3.3 Diagnostic Queries

```elixir
# Get errors for single declaration
def get_decl_diagnostics(state, decl_id)

# Get all errors in namespace
def get_namespace_diagnostics(state, namespace)

# Get workspace-wide errors
def get_all_diagnostics(state)
```

---

## Phase 4: LSP Protocol Bridge

### 4.1 LSP Server

**File: `nova_lang/lib/nova/lsp/server.ex`** (new)

```elixir
defmodule Nova.LSP.Server do
  # Standard LSP lifecycle
  def handle_request("initialize", params)
  def handle_request("shutdown", _)

  # Document sync (maps to namespace operations)
  def handle_notification("textDocument/didOpen", params)
  def handle_notification("textDocument/didChange", params)
  def handle_notification("textDocument/didClose", params)

  # Language features
  def handle_request("textDocument/hover", params)
  def handle_request("textDocument/completion", params)
  def handle_request("textDocument/definition", params)
  def handle_request("textDocument/references", params)
  def handle_request("textDocument/diagnostics", params)
end
```

### 4.2 URI to Namespace Mapping

```elixir
# Convert LSP URIs to namespace operations
defmodule Nova.LSP.URIMapper do
  # file:///project/src/MyApp/Core.purs → "MyApp.Core"
  def uri_to_namespace(uri)

  # "MyApp.Core" → file:///project/src/MyApp/Core.purs
  def namespace_to_uri(namespace, root)

  # Position in file → declaration ID
  def position_to_decl_id(state, uri, line, column)
end
```

---

## Phase 5: Advanced Features

### 5.1 Refactoring Support

```elixir
defmodule Nova.NamespaceService.Refactor do
  # Rename a declaration across all references
  def rename(state, decl_id, new_name)

  # Extract expression to new declaration
  def extract(state, decl_id, range, new_name)

  # Inline a declaration
  def inline(state, decl_id)

  # Move declaration to different namespace
  def move(state, decl_id, target_namespace)
end
```

### 5.2 Code Actions

```elixir
defmodule Nova.NamespaceService.CodeActions do
  # Fix missing import
  def add_import(state, namespace, name)

  # Add type signature
  def add_type_signature(state, decl_id)

  # Fix type error (if possible)
  def apply_fix(state, diagnostic_id, fix_kind)
end
```

### 5.3 Semantic Tokens (Syntax Highlighting)

```elixir
defmodule Nova.NamespaceService.SemanticTokens do
  # Get semantic token types for declaration
  def get_tokens(state, decl_id)
  # Returns: [{line, col, length, token_type, modifiers}]
end
```

---

## Data Structures Summary

### PureScript Side (Compiler Core)

```purescript
-- Ast.purs additions
type DeclId = String
type ManagedDecl = { meta :: DeclMetadata, decl :: Declaration, ... }

-- Types.purs additions
type NamespaceEnv = { env :: Env, cache :: TypeCache }
type ServiceState = { namespaces :: Map String NamespaceEnv }

-- New modules
module Nova.Compiler.Dependencies (getDependencies, buildGraph, getAffected)
module Nova.Compiler.Cache (TypeCache, isCacheValid, invalidate)
module Nova.Compiler.Incremental (checkSingleDecl, validateDirty)
```

### Elixir Side (Service Layer)

```elixir
# GenServer state
%Nova.NamespaceService{
  namespaces: %{namespace_id => %NamespaceState{}},
  global_counter: int
}

%NamespaceState{
  declarations: %{decl_id => managed_decl},
  dep_graph: :digraph.graph(),
  type_env: env_snapshot
}

# LSP Server
%Nova.LSP.Server{
  namespace_service: pid,
  client_capabilities: map,
  root_uri: string
}
```

---

## Implementation Order

1. **Foundation** (1-2 weeks)
   - [ ] Add DeclMetadata to Ast.purs
   - [ ] Implement getDependencies
   - [ ] Create Nova.NamespaceService GenServer skeleton

2. **Incremental Checking** (2-3 weeks)
   - [ ] Implement checkSingleDecl
   - [ ] Add caching layer
   - [ ] Implement invalidation logic

3. **Query API** (1-2 weeks)
   - [ ] Type queries
   - [ ] Diagnostic queries
   - [ ] Basic completions

4. **LSP Bridge** (2-3 weeks)
   - [ ] LSP Server skeleton
   - [ ] Document sync
   - [ ] Hover, completions, diagnostics

5. **Polish** (ongoing)
   - [ ] Go to definition
   - [ ] Find references
   - [ ] Refactoring support

---

## API Example Usage

```elixir
# Start service
{:ok, svc} = Nova.NamespaceService.start_link()

# Create namespace
:ok = Nova.NamespaceService.create_namespace(svc, "MyApp.Core")

# Add declarations
{:ok, id1} = Nova.NamespaceService.add_declaration(svc, "MyApp.Core", """
  data Color = Red | Green | Blue
""")

{:ok, id2} = Nova.NamespaceService.add_declaration(svc, "MyApp.Core", """
  colorName :: Color -> String
  colorName Red = "red"
  colorName Green = "green"
  colorName Blue = "blue"
""")

# Query
{:ok, type} = Nova.NamespaceService.get_type(svc, id2)
# => "Color -> String"

{:ok, diags} = Nova.NamespaceService.get_diagnostics(svc, "MyApp.Core")
# => []

# Update with error
{:ok, id2} = Nova.NamespaceService.update_declaration(svc, id2, """
  colorName :: Color -> Int  -- Wrong return type!
  colorName Red = "red"
""")

{:ok, diags} = Nova.NamespaceService.get_diagnostics(svc, "MyApp.Core")
# => [%{decl_id: id2, message: "Type mismatch: expected Int, got String", ...}]

# Remove
:ok = Nova.NamespaceService.remove_declaration(svc, id1)

# Check dependents are invalidated
{:ok, status} = Nova.NamespaceService.get_status(svc, id2)
# => :stale (because Color was removed)
```

---

## Questions to Resolve

1. **Persistence**: Should namespaces persist to disk? What format?
2. **Multi-user**: Should multiple clients share state?
3. **Versioning**: How to handle concurrent edits?
4. **Imports**: How do cross-namespace imports work?
5. **Code Gen**: When/how to generate Elixir code?
