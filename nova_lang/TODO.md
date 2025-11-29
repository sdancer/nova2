# Nova MCP Server - TODO

## High Priority

### Merge beam → main
The `beam` branch has major features ready. Should consolidate progress.

## Features

### Export Namespace to File
Write namespace declarations back to a `.purs` source file. Enables round-trip: load file → edit in namespace → save back.

```
export_namespace(namespace: "MyApp", path: "src/MyApp.purs")
```

### Auto-Checkpoint
Automatically create checkpoint before mutating operations (add/update/remove declaration). Would enable automatic undo without explicit checkpoint calls.

### Refactoring Tools
- `rename_declaration` - Rename a function/type and update all references
- `move_declaration` - Move declaration between namespaces
- `extract_function` - Extract expression into a new function
- `inline_function` - Inline a function at call sites

### Watch/Subscribe
Notify on namespace changes. Useful for reactive UIs or agent workflows that need to respond to changes.

```
subscribe(namespace: "MyApp", events: ["declaration_added", "validation_complete"])
```

### Better Diagnostics
Structure type errors with:
- Line numbers and column positions
- Source code snippets
- Suggested fixes
- Related declarations

### Multi-file Compilation
Compile multiple namespaces with proper dependency ordering to an output directory, preserving module structure.

### Type Hole Support
Add `_` as a type hole that returns the inferred type. Useful for agents exploring what type is expected.

```
add_declaration(source: "foo x = _ x")
# Returns: Expected type at hole: Int -> Int
```

## Infrastructure

### Debug MCP Health Check
`claude mcp list` shows "Failed to connect" but server works when tested directly. May be protocol version or timing issue.

### Reduce Escript Size
Current escript is ~2MB. Could slim down by excluding unused modules.

### Add MCP Tests
Add tests for MCP server integration - test JSON-RPC protocol compliance.

### Session Autosave
Periodically autosave session to prevent data loss on crash.

## Documentation

### Agent Workflow Examples
Document full workflows for AI agents:
- Building a module from scratch
- Refactoring existing code
- Test-driven development
- Debugging type errors

### MCP Tool Reference
Generate comprehensive documentation for all 31 MCP tools with examples.

## Completed Features

- [x] Namespace CRUD (create, delete, list)
- [x] Declaration management (add, update, remove, list, get)
- [x] Type checking and validation
- [x] Code intelligence (completions, get_type)
- [x] Imports between namespaces
- [x] File operations (load_file, compile_file, compile_namespace)
- [x] Compiler core operations (load_compiler_core, compile_compiler, validate_compiler)
- [x] Session persistence (save, load, list, delete sessions)
- [x] Undo/redo with checkpoints
- [x] Expression evaluation (eval)
- [x] Testing framework (run_tests, assert)
- [x] Claude Code MCP registration
- [x] Fix tokenizer bug (`10 20` was parsed as `10.20`)
