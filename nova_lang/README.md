# Nova

A PureScript-like language that compiles to Elixir/BEAM.

## MCP Server

Start the MCP server for AI-assisted development:

```bash
mix run --no-halt -e 'Nova.MCPMultiServer.start_link(port: 9999)'
```

### Sandbox Execution

The MCP server provides sandboxed BEAM subprocesses for safe code execution:

```
# Load compiler modules
load_compiler_core {source_dir: "/path/to/src/Nova/Compiler"}

# Start isolated sandbox
sandbox_start  →  {sandbox_id: "abc123"}

# Load runtime (required for library functions)
sandbox_eval {sandbox_id: "abc123", code: "Code.require_file(\"lib/nova/runtime.ex\")"}

# Load compiled modules into sandbox
sandbox_load {sandbox_id: "abc123", namespace: "Nova.Compiler.Tokenizer"}
sandbox_load {sandbox_id: "abc123", namespace: "Nova.Compiler.Parser"}
sandbox_load {sandbox_id: "abc123", namespace: "Nova.Compiler.CodeGen"}

# Execute code safely
sandbox_eval {sandbox_id: "abc123", code: "Nova.Compiler.Tokenizer.tokenize(\"x + 1\")"}

# Call functions directly
sandbox_call {sandbox_id: "abc123", module: "Nova.Compiler.CodeGen", function: "gen_module", args: [mod]}

# Cleanup
sandbox_stop {sandbox_id: "abc123"}
```

Sandboxes run in separate OS processes, isolating them from the main MCP server.
