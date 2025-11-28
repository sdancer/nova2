# Nova MCP Interface Plan

## Overview

Implement a Model Context Protocol (MCP) server for the Nova compiler, enabling AI assistants like Claude to interact with the compiler programmatically. The MCP server will wrap the existing `Nova.NamespaceService` GenServer and expose compilation, type-checking, and code management features.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     MCP Client (Claude)                        │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ stdio/JSON-RPC
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Nova.MCP.Server                              │
│  - Handles MCP protocol (tools, resources)                      │
│  - Translates MCP requests to NamespaceService calls            │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                Nova.NamespaceService (GenServer)                │
│  - Namespace/declaration management                             │
│  - Incremental type checking                                    │
│  - Dependency tracking                                          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│              Nova Compiler Pipeline                             │
│  Tokenizer → Parser → TypeChecker → CodeGen                     │
└─────────────────────────────────────────────────────────────────┘
```

## Implementation Steps

### Phase 1: Dependencies & Setup

1. Add MCP dependency to `mix.exs`:
   ```elixir
   defp deps do
     [
       {:mcp, "~> 0.1"}  # or appropriate MCP library
     ]
   end
   ```

2. Create basic MCP server module structure:
   - `lib/nova/mcp/server.ex` - Main MCP server
   - `lib/nova/mcp/tools.ex` - Tool definitions
   - `lib/nova/mcp/resources.ex` - Resource definitions (optional)

### Phase 2: Core MCP Tools

#### Namespace Management Tools

| Tool Name | Description | Parameters |
|-----------|-------------|------------|
| `nova_create_namespace` | Create a new namespace | `name: string` |
| `nova_delete_namespace` | Delete a namespace | `name: string` |
| `nova_list_namespaces` | List all namespaces | none |
| `nova_import_namespace` | Add import to namespace | `namespace: string, import: string` |

#### Declaration Management Tools

| Tool Name | Description | Parameters |
|-----------|-------------|------------|
| `nova_add_declaration` | Add code to namespace | `namespace: string, source: string` |
| `nova_update_declaration` | Update existing declaration | `decl_id: string, source: string` |
| `nova_remove_declaration` | Remove a declaration | `decl_id: string` |
| `nova_list_declarations` | List declarations in namespace | `namespace: string` |
| `nova_get_declaration` | Get declaration by name | `namespace: string, name: string` |

#### Type & Validation Tools

| Tool Name | Description | Parameters |
|-----------|-------------|------------|
| `nova_validate` | Type-check a namespace | `namespace: string` |
| `nova_get_type` | Get inferred type | `decl_id: string` |
| `nova_get_diagnostics` | Get errors for namespace | `namespace: string` |
| `nova_get_completions` | Get completions for prefix | `namespace: string, prefix: string` |

#### File Import Tools

| Tool Name | Description | Parameters |
|-----------|-------------|------------|
| `nova_import_file` | Load file into namespace | `path: string, namespace: string` |
| `nova_import_directory` | Load all .nova files from dir | `path: string, namespace_prefix: string` |

#### Compilation Tools

| Tool Name | Description | Parameters |
|-----------|-------------|------------|
| `nova_compile` | Compile source to Elixir | `source: string` |
| `nova_compile_file` | Compile file to Elixir | `input_path: string, output_path?: string` |
| `nova_compile_namespace` | Compile namespace to Elixir | `namespace: string` |
| `nova_generate_code` | Generate Elixir from validated namespace | `namespace: string` |

### Phase 3: File Import Implementation

```elixir
defmodule Nova.MCP.FileImporter do
  @doc """
  Import a Nova source file into a namespace.
  Parses the file and adds each declaration to the namespace.
  """
  def import_file(server, path, namespace) do
    with {:ok, source} <- File.read(path),
         tokens = Nova.Compiler.Tokenizer.tokenize(source),
         {:right, {:tuple, mod, _}} <- Nova.Compiler.Parser.parse_module(tokens) do

      # Create namespace if it doesn't exist
      Nova.NamespaceService.create_namespace(server, namespace)

      # Add each declaration
      results = Enum.map(mod.declarations, fn decl ->
        source_text = extract_decl_source(source, decl)
        Nova.NamespaceService.add_declaration(server, namespace, source_text)
      end)

      {:ok, results}
    end
  end

  @doc """
  Import all .nova files from a directory into namespaces.
  File path determines namespace: src/Foo/Bar.nova -> Prefix.Foo.Bar
  """
  def import_directory(server, dir_path, namespace_prefix) do
    Path.wildcard(Path.join(dir_path, "**/*.nova"))
    |> Enum.map(fn file_path ->
      namespace = file_to_namespace(file_path, dir_path, namespace_prefix)
      import_file(server, file_path, namespace)
    end)
  end
end
```

### Phase 4: MCP Server Implementation

```elixir
defmodule Nova.MCP.Server do
  @behaviour MCP.Server

  def init(_args) do
    {:ok, svc} = Nova.NamespaceService.start_link()
    {:ok, %{namespace_service: svc}}
  end

  def handle_tool_call("nova_create_namespace", %{"name" => name}, state) do
    result = Nova.NamespaceService.create_namespace(state.namespace_service, name)
    {:ok, format_result(result), state}
  end

  def handle_tool_call("nova_add_declaration", params, state) do
    %{"namespace" => ns, "source" => src} = params
    result = Nova.NamespaceService.add_declaration(state.namespace_service, ns, src)
    {:ok, format_result(result), state}
  end

  # ... other tool handlers

  def list_tools do
    [
      %{name: "nova_create_namespace", description: "...", inputSchema: %{...}},
      %{name: "nova_add_declaration", description: "...", inputSchema: %{...}},
      # ...
    ]
  end
end
```

### Phase 5: CLI Entry Point

Add MCP server mode to CLI:

```elixir
# In lib/nova/cli.ex
def main(["mcp" | _args]) do
  Nova.MCP.Server.start()
end
```

Usage: `nova mcp` or via escript.

## MCP Configuration

Example MCP client configuration (for Claude Code):

```json
{
  "mcpServers": {
    "nova": {
      "command": "mix",
      "args": ["run", "--no-halt", "-e", "Nova.MCP.Server.start()"],
      "cwd": "/path/to/nova_lang"
    }
  }
}
```

Or with escript:

```json
{
  "mcpServers": {
    "nova": {
      "command": "/path/to/nova_lang/nova",
      "args": ["mcp"]
    }
  }
}
```

## Testing Plan

### Unit Tests

1. Test each MCP tool handler in isolation
2. Test file import functionality
3. Test namespace-to-namespace imports

### Integration Tests

1. Start MCP server, send JSON-RPC requests, verify responses
2. Test full workflow: create namespace → import file → validate → compile

### Manual Testing with Claude Code

1. Configure MCP server in Claude Code settings
2. Test via prompts:
   - "Create a namespace called MyApp"
   - "Add a function `add x y = x + y` to MyApp"
   - "Validate the MyApp namespace"
   - "What's the type of add?"
   - "Compile the namespace to Elixir"

## Implementation Status

### Completed
- [x] MCP server module with stdio transport (`lib/nova/mcp/server.ex`)
- [x] Tool definitions for all operations (`lib/nova/mcp/tools.ex`)
- [x] File import utilities (`lib/nova/mcp/file_importer.ex`)
- [x] Namespace compilation utilities (`lib/nova/mcp/compiler.ex`)
- [x] CLI integration (`nova mcp` command)
- [x] Basic tests passing for namespace management

### Fully Working (After PureScript Build)
All tools are functional after regenerating the compiler:
- `nova_add_declaration` - Parses and adds declarations
- `nova_compile` - Full compilation pipeline working
- `nova_compile_namespace` - Generates Elixir from validated namespace
- `nova_parse` - Tokenizes and parses Nova source
- `nova_validate` - Type checks declarations
- `nova_import_file` - Imports files into namespaces

### Build Commands
```bash
# Install dependencies (local)
npm install purescript spago

# Build PureScript compiler
npx spago build

# Regenerate Elixir modules
node scripts/regenerate.js

# Recompile Elixir
cd nova_lang && mix compile
```

## Files Created

```
nova_lang/lib/nova/mcp/
├── server.ex        # MCP server with stdio transport
├── tools.ex         # Tool definitions and handlers
├── file_importer.ex # File import utilities
└── compiler.ex      # Namespace compilation
```

## Example Session

```
Human: Create a namespace for my math utilities

Claude: [calls nova_create_namespace with name="Math.Utils"]
Created namespace Math.Utils