defmodule Nova.MCPServer do
  @moduledoc """
  MCP (Model Context Protocol) server for the Nova compiler.

  Exposes namespace CRUD operations as MCP tools, allowing AI assistants
  and other MCP clients to interact with the Nova compiler service.

  ## Available Tools

  ### Namespace Management
  - `create_namespace` - Create a new namespace
  - `delete_namespace` - Delete a namespace and all its declarations
  - `list_namespaces` - List all namespaces

  ### Declaration Management
  - `add_declaration` - Add a declaration to a namespace
  - `update_declaration` - Update an existing declaration
  - `remove_declaration` - Remove a declaration
  - `list_declarations` - List all declarations in a namespace
  - `get_declaration` - Get a declaration by name

  ### Type Checking
  - `validate_namespace` - Type check all declarations in a namespace
  - `get_type` - Get the inferred type of a declaration
  - `get_diagnostics` - Get type errors and warnings

  ### Code Intelligence
  - `get_completions` - Get code completions for a prefix

  ## Usage

      {:ok, server} = ExMCP.Server.start_link(
        handler: Nova.MCPServer,
        transport: :stdio
      )
  """

  use ExMCP.Server.Handler

  # ============================================================================
  # Initialization
  # ============================================================================

  @impl true
  def init(_args) do
    # Start the namespace service
    {:ok, svc} = Nova.NamespaceService.start_link()
    {:ok, %{
      namespace_service: svc,
      undo_stack: [],      # Stack of previous states for undo
      redo_stack: [],      # Stack of undone states for redo
      max_history: 50      # Maximum undo history size
    }}
  end

  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      server_info: %{
        name: "nova-compiler",
        version: "0.1.0"
      },
      capabilities: %{
        tools: %{}
      }
    }, state}
  end

  # ============================================================================
  # Tool Definitions
  # ============================================================================

  @impl true
  def handle_list_tools(state) do
    tools = [
      # Namespace management
      %{
        name: "create_namespace",
        description: "Create a new namespace for organizing code declarations",
        inputSchema: %{
          type: "object",
          properties: %{
            "name" => %{
              type: "string",
              description: "The namespace name (e.g., 'MyApp.Core')"
            }
          },
          required: ["name"]
        }
      },
      %{
        name: "delete_namespace",
        description: "Delete a namespace and all its declarations",
        inputSchema: %{
          type: "object",
          properties: %{
            "name" => %{
              type: "string",
              description: "The namespace name to delete"
            }
          },
          required: ["name"]
        }
      },
      %{
        name: "list_namespaces",
        description: "List all existing namespaces",
        inputSchema: %{
          type: "object",
          properties: %{}
        }
      },

      # Declaration management
      %{
        name: "add_declaration",
        description: "Add a code declaration (function, data type, type alias, etc.) to a namespace",
        inputSchema: %{
          type: "object",
          properties: %{
            "namespace" => %{
              type: "string",
              description: "The namespace to add the declaration to"
            },
            "source" => %{
              type: "string",
              description: "The source code of the declaration (e.g., 'add x y = x + y')"
            }
          },
          required: ["namespace", "source"]
        }
      },
      %{
        name: "update_declaration",
        description: "Update an existing declaration with new source code",
        inputSchema: %{
          type: "object",
          properties: %{
            "decl_id" => %{
              type: "string",
              description: "The declaration ID to update"
            },
            "source" => %{
              type: "string",
              description: "The new source code"
            }
          },
          required: ["decl_id", "source"]
        }
      },
      %{
        name: "remove_declaration",
        description: "Remove a declaration from its namespace",
        inputSchema: %{
          type: "object",
          properties: %{
            "decl_id" => %{
              type: "string",
              description: "The declaration ID to remove"
            }
          },
          required: ["decl_id"]
        }
      },
      %{
        name: "list_declarations",
        description: "List all declarations in a namespace",
        inputSchema: %{
          type: "object",
          properties: %{
            "namespace" => %{
              type: "string",
              description: "The namespace to list declarations from"
            }
          },
          required: ["namespace"]
        }
      },
      %{
        name: "get_declaration",
        description: "Get a specific declaration by name in a namespace",
        inputSchema: %{
          type: "object",
          properties: %{
            "namespace" => %{
              type: "string",
              description: "The namespace containing the declaration"
            },
            "name" => %{
              type: "string",
              description: "The name of the declaration"
            }
          },
          required: ["namespace", "name"]
        }
      },

      # Type checking
      %{
        name: "validate_namespace",
        description: "Type check all declarations in a namespace and return any errors",
        inputSchema: %{
          type: "object",
          properties: %{
            "namespace" => %{
              type: "string",
              description: "The namespace to validate"
            }
          },
          required: ["namespace"]
        }
      },
      %{
        name: "get_type",
        description: "Get the inferred type of a declaration",
        inputSchema: %{
          type: "object",
          properties: %{
            "decl_id" => %{
              type: "string",
              description: "The declaration ID to get the type for"
            }
          },
          required: ["decl_id"]
        }
      },
      %{
        name: "get_expression_type",
        description: "Infer the type of an expression in a namespace context. Useful for understanding what type is expected or returned.",
        inputSchema: %{
          type: "object",
          properties: %{
            "expression" => %{
              type: "string",
              description: "The expression to type check (e.g., 'map show', 'x + 1', '\\x -> x')"
            },
            "namespace" => %{
              type: "string",
              description: "Optional namespace for context (provides access to its declarations)"
            }
          },
          required: ["expression"]
        }
      },
      %{
        name: "get_diagnostics",
        description: "Get all type errors and warnings for a namespace",
        inputSchema: %{
          type: "object",
          properties: %{
            "namespace" => %{
              type: "string",
              description: "The namespace to get diagnostics for"
            }
          },
          required: ["namespace"]
        }
      },

      # Code intelligence
      %{
        name: "get_completions",
        description: "Get code completions for a prefix in a namespace",
        inputSchema: %{
          type: "object",
          properties: %{
            "namespace" => %{
              type: "string",
              description: "The namespace context for completions"
            },
            "prefix" => %{
              type: "string",
              description: "The prefix to complete"
            }
          },
          required: ["namespace", "prefix"]
        }
      },

      # Imports
      %{
        name: "add_import",
        description: "Add an import from one namespace to another",
        inputSchema: %{
          type: "object",
          properties: %{
            "namespace" => %{
              type: "string",
              description: "The namespace that will import"
            },
            "imported_namespace" => %{
              type: "string",
              description: "The namespace to import from"
            }
          },
          required: ["namespace", "imported_namespace"]
        }
      },
      %{
        name: "list_imports",
        description: "List all imports of a namespace",
        inputSchema: %{
          type: "object",
          properties: %{
            "namespace" => %{
              type: "string",
              description: "The namespace to list imports for"
            }
          },
          required: ["namespace"]
        }
      },

      # File operations
      %{
        name: "load_file",
        description: "Load and parse a .purs source file into a namespace",
        inputSchema: %{
          type: "object",
          properties: %{
            "path" => %{
              type: "string",
              description: "Path to the .purs file"
            },
            "namespace" => %{
              type: "string",
              description: "Optional namespace name (defaults to module name from file)"
            }
          },
          required: ["path"]
        }
      },
      %{
        name: "compile_file",
        description: "Compile a .purs source file to Elixir code",
        inputSchema: %{
          type: "object",
          properties: %{
            "path" => %{
              type: "string",
              description: "Path to the .purs file"
            }
          },
          required: ["path"]
        }
      },
      %{
        name: "compile_namespace",
        description: "Generate Elixir code from all declarations in a namespace",
        inputSchema: %{
          type: "object",
          properties: %{
            "namespace" => %{
              type: "string",
              description: "The namespace to compile"
            }
          },
          required: ["namespace"]
        }
      },

      # Compiler core operations
      %{
        name: "load_compiler_core",
        description: "Load all 8 Nova compiler modules into namespaces (Ast, Types, Tokenizer, Parser, Unify, TypeChecker, Dependencies, CodeGen)",
        inputSchema: %{
          type: "object",
          properties: %{
            "source_dir" => %{
              type: "string",
              description: "Path to the Nova compiler source directory (default: ../src/Nova/Compiler)"
            }
          }
        }
      },
      %{
        name: "compile_compiler",
        description: "Compile all loaded compiler modules to Elixir and optionally write to output directory",
        inputSchema: %{
          type: "object",
          properties: %{
            "output_dir" => %{
              type: "string",
              description: "Output directory for compiled .ex files (optional - if omitted, returns code without writing)"
            }
          }
        }
      },
      %{
        name: "validate_compiler",
        description: "Type check all compiler modules and report any errors",
        inputSchema: %{
          type: "object",
          properties: %{}
        }
      },
      %{
        name: "compile_project",
        description: "Compile multiple namespaces to Elixir files with proper dependency ordering",
        inputSchema: %{
          type: "object",
          properties: %{
            "namespaces" => %{
              type: "array",
              items: %{type: "string"},
              description: "List of namespaces to compile (if omitted, compiles all namespaces)"
            },
            "output_dir" => %{
              type: "string",
              description: "Output directory for compiled .ex files (required)"
            },
            "validate" => %{
              type: "boolean",
              description: "Whether to validate (type check) before compiling (default: true)"
            }
          },
          required: ["output_dir"]
        }
      },

      # Session persistence
      %{
        name: "save_session",
        description: "Save current session state (all namespaces and declarations) to disk",
        inputSchema: %{
          type: "object",
          properties: %{
            "name" => %{
              type: "string",
              description: "Session name (used as filename)"
            },
            "description" => %{
              type: "string",
              description: "Optional description of the session"
            },
            "directory" => %{
              type: "string",
              description: "Directory to save session (default: .nova_sessions)"
            }
          },
          required: ["name"]
        }
      },
      %{
        name: "load_session",
        description: "Load a previously saved session, restoring all namespaces and declarations",
        inputSchema: %{
          type: "object",
          properties: %{
            "name" => %{
              type: "string",
              description: "Session name to load"
            },
            "directory" => %{
              type: "string",
              description: "Directory containing sessions (default: .nova_sessions)"
            },
            "merge" => %{
              type: "boolean",
              description: "If true, merge with existing namespaces instead of replacing (default: false)"
            }
          },
          required: ["name"]
        }
      },
      %{
        name: "list_sessions",
        description: "List all saved sessions",
        inputSchema: %{
          type: "object",
          properties: %{
            "directory" => %{
              type: "string",
              description: "Directory containing sessions (default: .nova_sessions)"
            }
          }
        }
      },
      %{
        name: "delete_session",
        description: "Delete a saved session",
        inputSchema: %{
          type: "object",
          properties: %{
            "name" => %{
              type: "string",
              description: "Session name to delete"
            },
            "directory" => %{
              type: "string",
              description: "Directory containing sessions (default: .nova_sessions)"
            }
          },
          required: ["name"]
        }
      },

      # Evaluation
      %{
        name: "eval",
        description: "Evaluate a Nova expression and return the result. Can use functions from loaded namespaces.",
        inputSchema: %{
          type: "object",
          properties: %{
            "expression" => %{
              type: "string",
              description: "The Nova expression to evaluate (e.g., '1 + 2', 'map (\\x -> x * 2) [1,2,3]')"
            },
            "namespace" => %{
              type: "string",
              description: "Optional namespace context for accessing declarations"
            }
          },
          required: ["expression"]
        }
      },

      # Undo/Redo
      %{
        name: "checkpoint",
        description: "Create a checkpoint of current state that can be restored with undo",
        inputSchema: %{
          type: "object",
          properties: %{
            "description" => %{
              type: "string",
              description: "Optional description of this checkpoint"
            }
          }
        }
      },
      %{
        name: "undo",
        description: "Undo the last change by restoring previous checkpoint",
        inputSchema: %{
          type: "object",
          properties: %{}
        }
      },
      %{
        name: "redo",
        description: "Redo a previously undone change",
        inputSchema: %{
          type: "object",
          properties: %{}
        }
      },
      %{
        name: "list_checkpoints",
        description: "List available checkpoints in undo history",
        inputSchema: %{
          type: "object",
          properties: %{}
        }
      },

      # Testing
      %{
        name: "run_tests",
        description: "Run all test functions in a namespace. Tests are functions named test_* that return true for pass or false/error for fail.",
        inputSchema: %{
          type: "object",
          properties: %{
            "namespace" => %{
              type: "string",
              description: "The namespace to run tests in"
            },
            "pattern" => %{
              type: "string",
              description: "Optional pattern to filter tests (default: test_)"
            }
          },
          required: ["namespace"]
        }
      },
      %{
        name: "assert",
        description: "Evaluate an expression and check if it equals expected value. Returns pass/fail result.",
        inputSchema: %{
          type: "object",
          properties: %{
            "expression" => %{
              type: "string",
              description: "The expression to evaluate"
            },
            "expected" => %{
              type: "string",
              description: "The expected result (as a string representation)"
            },
            "namespace" => %{
              type: "string",
              description: "Optional namespace context"
            },
            "description" => %{
              type: "string",
              description: "Optional description of what this assertion tests"
            }
          },
          required: ["expression", "expected"]
        }
      }
    ]

    {:ok, tools, state}
  end

  # ============================================================================
  # Tool Implementations
  # ============================================================================

  @impl true
  def handle_call_tool(name, args, state) do
    svc = state.namespace_service

    result = case name do
      # Namespace management
      "create_namespace" ->
        case Nova.NamespaceService.create_namespace(svc, args["name"]) do
          :ok -> {:ok, "Namespace '#{args["name"]}' created successfully"}
          {:error, :already_exists} -> {:error, "Namespace '#{args["name"]}' already exists"}
        end

      "delete_namespace" ->
        case Nova.NamespaceService.delete_namespace(svc, args["name"]) do
          :ok -> {:ok, "Namespace '#{args["name"]}' deleted successfully"}
          {:error, :not_found} -> {:error, "Namespace '#{args["name"]}' not found"}
        end

      "list_namespaces" ->
        {:ok, namespaces} = Nova.NamespaceService.list_namespaces(svc)
        {:ok, namespaces}

      # Declaration management
      "add_declaration" ->
        case Nova.NamespaceService.add_declaration(svc, args["namespace"], args["source"]) do
          {:ok, decl_id} -> {:ok, %{decl_id: decl_id, message: "Declaration added successfully"}}
          {:error, reason} -> {:error, format_error(reason)}
        end

      "update_declaration" ->
        case Nova.NamespaceService.update_declaration(svc, args["decl_id"], args["source"]) do
          {:ok, _} -> {:ok, "Declaration updated successfully"}
          {:error, reason} -> {:error, format_error(reason)}
        end

      "remove_declaration" ->
        case Nova.NamespaceService.remove_declaration(svc, args["decl_id"]) do
          :ok -> {:ok, "Declaration removed successfully"}
          {:error, reason} -> {:error, format_error(reason)}
        end

      "list_declarations" ->
        case Nova.NamespaceService.list_declarations(svc, args["namespace"]) do
          {:ok, decls} ->
            formatted = Enum.map(decls, fn decl ->
              %{
                id: decl.decl_id,
                name: decl.name,
                kind: to_string(decl.kind),
                status: to_string(decl.status)
              }
            end)
            {:ok, formatted}
          {:error, reason} -> {:error, format_error(reason)}
        end

      "get_declaration" ->
        case Nova.NamespaceService.get_declaration(svc, args["namespace"], args["name"]) do
          {:ok, decl} ->
            {:ok, %{
              id: decl.meta.decl_id,
              name: decl.meta.name,
              kind: to_string(decl.meta.kind),
              source: decl.source_text,
              type: format_type(decl.inferred_type),
              errors: decl.errors
            }}
          {:error, reason} -> {:error, format_error(reason)}
        end

      # Type checking
      "validate_namespace" ->
        case Nova.NamespaceService.validate_namespace(svc, args["namespace"]) do
          {:ok, _} -> {:ok, "Namespace '#{args["namespace"]}' validated successfully (no errors)"}
          {:error, {:type_errors, errors}} -> {:error, "Type errors: #{inspect(errors)}"}
          {:error, reason} -> {:error, format_error(reason)}
        end

      "get_type" ->
        case Nova.NamespaceService.get_type(svc, args["decl_id"]) do
          {:ok, type} -> {:ok, format_type(type)}
          {:error, reason} -> {:error, format_error(reason)}
        end

      "get_expression_type" ->
        get_expression_type(svc, args["expression"], args["namespace"])

      "get_diagnostics" ->
        case Nova.NamespaceService.get_diagnostics(svc, args["namespace"]) do
          {:ok, diagnostics} -> {:ok, diagnostics}
          {:error, reason} -> {:error, format_error(reason)}
        end

      # Code intelligence
      "get_completions" ->
        case Nova.NamespaceService.get_completions(svc, args["namespace"], args["prefix"]) do
          {:ok, completions} -> {:ok, completions}
          {:error, reason} -> {:error, format_error(reason)}
        end

      # Imports
      "add_import" ->
        case Nova.NamespaceService.add_import(svc, args["namespace"], args["imported_namespace"]) do
          :ok -> {:ok, "Import added successfully"}
          {:error, reason} -> {:error, format_error(reason)}
        end

      "list_imports" ->
        case Nova.NamespaceService.list_imports(svc, args["namespace"]) do
          {:ok, imports} -> {:ok, imports}
          {:error, reason} -> {:error, format_error(reason)}
        end

      # File operations
      "load_file" ->
        load_file(svc, args["path"], args["namespace"])

      "compile_file" ->
        compile_file(args["path"])

      "compile_namespace" ->
        compile_namespace(svc, args["namespace"])

      # Compiler core operations
      "load_compiler_core" ->
        load_compiler_core(svc, args["source_dir"])

      "compile_compiler" ->
        compile_compiler(args["output_dir"])

      "validate_compiler" ->
        validate_compiler(svc)

      "compile_project" ->
        compile_project(svc, args["namespaces"], args["output_dir"], args["validate"])

      # Session persistence
      "save_session" ->
        save_session(svc, args["name"], args["description"], args["directory"])

      "load_session" ->
        load_session(svc, args["name"], args["directory"], args["merge"])

      "list_sessions" ->
        list_sessions(args["directory"])

      "delete_session" ->
        delete_session(args["name"], args["directory"])

      # Evaluation
      "eval" ->
        eval_expression(svc, args["expression"], args["namespace"])

      # Undo/Redo
      "checkpoint" ->
        {:ok_with_state, create_checkpoint(state, args["description"])}

      "undo" ->
        case undo(state) do
          {:ok, new_state, info} -> {:ok_with_state, {new_state, info}}
          {:error, reason} -> {:error, reason}
        end

      "redo" ->
        case redo(state) do
          {:ok, new_state, info} -> {:ok_with_state, {new_state, info}}
          {:error, reason} -> {:error, reason}
        end

      "list_checkpoints" ->
        {:ok, list_checkpoints(state)}

      # Testing
      "run_tests" ->
        run_tests(svc, args["namespace"], args["pattern"])

      "assert" ->
        run_assert(svc, args["expression"], args["expected"], args["namespace"], args["description"])

      _ ->
        {:error, "Unknown tool: #{name}"}
    end

    case result do
      {:ok_with_state, {new_state, data}} ->
        {:ok, [%{type: "text", text: format_result(data)}], new_state}
      {:ok, data} ->
        {:ok, [%{type: "text", text: format_result(data)}], state}
      {:error, msg} ->
        {:ok, [%{type: "text", text: "Error: #{msg}"}], state}
    end
  end

  # ============================================================================
  # Helpers
  # ============================================================================

  defp format_result(data) when is_binary(data), do: data
  defp format_result(data) when is_list(data), do: Jason.encode!(data, pretty: true)
  defp format_result(data) when is_map(data), do: Jason.encode!(data, pretty: true)
  defp format_result(data), do: inspect(data)

  defp format_error(reason) when is_binary(reason), do: reason
  defp format_error(reason), do: inspect(reason)

  defp format_type(nil), do: "unknown"
  defp format_type(type), do: pretty_type(type)

  # Pretty-print types in human-readable format
  defp pretty_type({:ty_var, v}), do: v.name

  defp pretty_type({:ty_con, %{name: "Fun", args: [arg, ret]}}) do
    arg_str = case arg do
      {:ty_con, %{name: "Fun", args: _}} -> "(#{pretty_type(arg)})"
      _ -> pretty_type(arg)
    end
    "#{arg_str} -> #{pretty_type(ret)}"
  end

  defp pretty_type({:ty_con, %{name: "Array", args: [elem]}}) do
    "Array #{pretty_type_arg(elem)}"
  end

  defp pretty_type({:ty_con, %{name: "List", args: [elem]}}) do
    "List #{pretty_type_arg(elem)}"
  end

  defp pretty_type({:ty_con, %{name: "Maybe", args: [elem]}}) do
    "Maybe #{pretty_type_arg(elem)}"
  end

  defp pretty_type({:ty_con, %{name: "Either", args: [l, r]}}) do
    "Either #{pretty_type_arg(l)} #{pretty_type_arg(r)}"
  end

  defp pretty_type({:ty_con, %{name: "Map", args: [k, v]}}) do
    "Map #{pretty_type_arg(k)} #{pretty_type_arg(v)}"
  end

  defp pretty_type({:ty_con, %{name: "Set", args: [elem]}}) do
    "Set #{pretty_type_arg(elem)}"
  end

  defp pretty_type({:ty_con, %{name: "Tuple", args: elems}}) do
    inner = Enum.map_join(elems, ", ", &pretty_type/1)
    "(#{inner})"
  end

  defp pretty_type({:ty_con, %{name: name, args: []}}) do
    name
  end

  defp pretty_type({:ty_con, %{name: name, args: args}}) do
    args_str = Enum.map_join(args, " ", &pretty_type_arg/1)
    "#{name} #{args_str}"
  end

  defp pretty_type({:ty_record, %{fields: fields, row: row}}) do
    fields_str = fields
      |> Nova.Map.to_unfoldable()
      |> Enum.map_join(", ", fn {:tuple, name, ty} -> "#{name} :: #{pretty_type(ty)}" end)
    case row do
      :nothing -> "{ #{fields_str} }"
      {:just, r} -> "{ #{fields_str} | #{r} }"
    end
  end

  defp pretty_type(other), do: inspect(other)

  # Wrap complex types in parens when used as arguments
  defp pretty_type_arg({:ty_con, %{name: "Fun", args: _}} = ty), do: "(#{pretty_type(ty)})"
  defp pretty_type_arg({:ty_con, %{name: _, args: args}} = ty) when length(args) > 0, do: "(#{pretty_type(ty)})"
  defp pretty_type_arg(ty), do: pretty_type(ty)

  # File operations

  defp load_file(svc, path, namespace_override) do
    case File.read(path) do
      {:ok, source} ->
        tokens = Nova.Compiler.Tokenizer.tokenize(source)

        case Nova.Compiler.Parser.parse_module(tokens) do
          {:right, {:tuple, mod, _rest}} ->
            namespace = namespace_override || mod.name

            # Create namespace if it doesn't exist
            Nova.NamespaceService.create_namespace(svc, namespace)

            # Add each declaration
            results = Enum.map(mod.declarations, fn decl ->
              # Convert declaration back to source (simplified - just use inspect for now)
              decl_source = format_declaration_source(decl)
              case Nova.NamespaceService.add_declaration(svc, namespace, decl_source) do
                {:ok, id} -> {:ok, id}
                {:error, reason} -> {:error, reason}
              end
            end)

            ok_count = Enum.count(results, fn {status, _} -> status == :ok end)
            error_count = Enum.count(results, fn {status, _} -> status == :error end)

            {:ok, %{
              namespace: namespace,
              module_name: mod.name,
              declarations_loaded: ok_count,
              errors: error_count,
              path: path
            }}

          {:left, error} ->
            {:error, "Parse error: #{inspect(error)}"}
        end

      {:error, reason} ->
        {:error, "Failed to read file: #{reason}"}
    end
  end

  defp compile_file(path) do
    case File.read(path) do
      {:ok, source} ->
        tokens = Nova.Compiler.Tokenizer.tokenize(source)

        case Nova.Compiler.Parser.parse_module(tokens) do
          {:right, {:tuple, mod, _rest}} ->
            code = Nova.Compiler.CodeGen.gen_module(mod)
            {:ok, %{
              module_name: mod.name,
              elixir_code: code,
              lines: length(String.split(code, "\n"))
            }}

          {:left, error} ->
            {:error, "Parse error: #{inspect(error)}"}
        end

      {:error, reason} ->
        {:error, "Failed to read file: #{reason}"}
    end
  end

  defp compile_namespace(svc, namespace) do
    case Nova.NamespaceService.list_declarations(svc, namespace) do
      {:ok, decls} ->
        # Build a module AST from the declarations
        declarations = Enum.flat_map(decls, fn decl_info ->
          case Nova.NamespaceService.get_declaration(svc, namespace, decl_info.name) do
            {:ok, managed_decl} -> [managed_decl.decl]
            _ -> []
          end
        end)

        mod = %{name: namespace, declarations: declarations}
        code = Nova.Compiler.CodeGen.gen_module(mod)

        {:ok, %{
          namespace: namespace,
          elixir_code: code,
          lines: length(String.split(code, "\n")),
          declarations: length(decls)
        }}

      {:error, reason} ->
        {:error, format_error(reason)}
    end
  end

  # Format a declaration back to source code (simplified)
  defp format_declaration_source(decl) do
    case decl do
      {:decl_function, func} ->
        params = Enum.map(func.parameters, &format_pattern/1) |> Enum.join(" ")
        "#{func.name} #{params} = #{format_expr(func.body)}"

      {:decl_data_type, dt} ->
        ctors = Enum.map(dt.constructors, fn ctor ->
          if Enum.empty?(ctor.fields) do
            ctor.name
          else
            fields = Enum.map(ctor.fields, fn f -> format_type_expr(f.ty) end) |> Enum.join(" ")
            "#{ctor.name} #{fields}"
          end
        end) |> Enum.join(" | ")
        type_vars = if Enum.empty?(dt.typeVars), do: "", else: " " <> Enum.join(dt.typeVars, " ")
        "data #{dt.name}#{type_vars} = #{ctors}"

      {:decl_type_alias, ta} ->
        type_vars = if Enum.empty?(ta.typeVars), do: "", else: " " <> Enum.join(ta.typeVars, " ")
        "type #{ta.name}#{type_vars} = #{format_type_expr(ta.ty)}"

      {:decl_type_sig, sig} ->
        "#{sig.name} :: #{format_type_expr(sig.ty)}"

      _ ->
        "-- unsupported declaration"
    end
  end

  defp format_pattern({:pat_var, name}), do: name
  defp format_pattern({:pat_wildcard}), do: "_"
  defp format_pattern({:pat_literal, {:lit_int, n}}), do: to_string(n)
  defp format_pattern({:pat_literal, {:lit_string, s}}), do: "\"#{s}\""
  defp format_pattern({:pat_constructor, name, args}) do
    if Enum.empty?(args), do: name, else: "(#{name} #{Enum.map(args, &format_pattern/1) |> Enum.join(" ")})"
  end
  defp format_pattern(_), do: "_"

  defp format_expr({:expr_var, name}), do: name
  defp format_expr({:expr_literal, {:lit_int, n}}), do: to_string(n)
  defp format_expr({:expr_literal, {:lit_string, s}}), do: "\"#{s}\""
  defp format_expr({:expr_app, f, arg}), do: "(#{format_expr(f)} #{format_expr(arg)})"
  defp format_expr({:expr_lambda, params, body}) do
    ps = Enum.map(params, &format_pattern/1) |> Enum.join(" ")
    "(\\#{ps} -> #{format_expr(body)})"
  end
  defp format_expr({:expr_infix, op, l, r}), do: "(#{format_expr(l)} #{op} #{format_expr(r)})"
  defp format_expr(_), do: "..."

  defp format_type_expr({:ty_expr_con, name}), do: name
  defp format_type_expr({:ty_expr_var, name}), do: name
  defp format_type_expr({:ty_expr_app, f, arg}), do: "(#{format_type_expr(f)} #{format_type_expr(arg)})"
  defp format_type_expr({:ty_expr_arrow, a, b}), do: "(#{format_type_expr(a)} -> #{format_type_expr(b)})"
  defp format_type_expr({:ty_expr_record, fields, _row}) do
    fs = Enum.map(fields, fn {name, ty} -> "#{name} :: #{format_type_expr(ty)}" end) |> Enum.join(", ")
    "{ #{fs} }"
  end
  defp format_type_expr(_), do: "?"

  # ============================================================================
  # Compiler Core Operations
  # ============================================================================

  @compiler_modules [
    "Ast",
    "Types",
    "Tokenizer",
    "Parser",
    "Unify",
    "TypeChecker",
    "Dependencies",
    "CodeGen"
  ]

  defp load_compiler_core(svc, source_dir) do
    base_dir = source_dir || "../src/Nova/Compiler"

    results = Enum.map(@compiler_modules, fn mod_name ->
      path = Path.join(base_dir, "#{mod_name}.purs")
      namespace = "Nova.Compiler.#{mod_name}"

      case File.read(path) do
        {:ok, source} ->
          tokens = Nova.Compiler.Tokenizer.tokenize(source)

          case Nova.Compiler.Parser.parse_module(tokens) do
            {:right, {:tuple, mod, _rest}} ->
              # Create namespace
              Nova.NamespaceService.create_namespace(svc, namespace)

              # Store the parsed module in the namespace metadata
              # For now, we'll store it in an ETS table or process state
              # Actually, let's store the source and parsed module
              decl_count = length(mod.declarations)

              # Store module AST for later compilation
              :persistent_term.put({:nova_module, namespace}, mod)

              {:ok, %{module: mod_name, namespace: namespace, declarations: decl_count}}

            {:left, error} ->
              {:error, %{module: mod_name, error: "Parse error: #{inspect(error)}"}}
          end

        {:error, reason} ->
          {:error, %{module: mod_name, error: "File not found: #{reason}"}}
      end
    end)

    successes = Enum.filter(results, fn {status, _} -> status == :ok end)
    failures = Enum.filter(results, fn {status, _} -> status == :error end)

    {:ok, %{
      loaded: length(successes),
      failed: length(failures),
      modules: Enum.map(successes, fn {:ok, info} -> info end),
      errors: Enum.map(failures, fn {:error, info} -> info end)
    }}
  end

  defp compile_compiler(output_dir) do
    # Collect all module declarations for dependency resolution
    all_dep_decls = Enum.flat_map(@compiler_modules, fn mod_name ->
      namespace = "Nova.Compiler.#{mod_name}"
      case :persistent_term.get({:nova_module, namespace}, nil) do
        nil -> []
        mod -> mod.declarations
      end
    end)

    results = Enum.map(@compiler_modules, fn mod_name ->
      namespace = "Nova.Compiler.#{mod_name}"

      case :persistent_term.get({:nova_module, namespace}, nil) do
        nil ->
          {:error, %{module: mod_name, error: "Module not loaded. Run load_compiler_core first."}}

        mod ->
          # Pass all other module declarations as dependencies
          other_decls = Enum.flat_map(@compiler_modules, fn other_name ->
            if other_name == mod_name do
              []
            else
              other_ns = "Nova.Compiler.#{other_name}"
              case :persistent_term.get({:nova_module, other_ns}, nil) do
                nil -> []
                other_mod -> other_mod.declarations
              end
            end
          end)

          case Nova.compile_module(mod, other_decls) do
            {:ok, code} ->
              lines = length(String.split(code, "\n"))

              # Optionally write to file
              if output_dir do
                File.mkdir_p!(output_dir)
                output_path = Path.join(output_dir, "#{mod_name}.ex")
                File.write!(output_path, code)
                {:ok, %{module: mod_name, lines: lines, path: output_path}}
              else
                {:ok, %{module: mod_name, lines: lines}}
              end

            {:error, reason} ->
              {:error, %{module: mod_name, error: inspect(reason)}}
          end
      end
    end)

    successes = Enum.filter(results, fn {status, _} -> status == :ok end)
    failures = Enum.filter(results, fn {status, _} -> status == :error end)

    total_lines = Enum.reduce(successes, 0, fn {:ok, info}, acc -> acc + Map.get(info, :lines, 0) end)

    {:ok, %{
      compiled: length(successes),
      failed: length(failures),
      total_lines: total_lines,
      output_dir: output_dir,
      modules: Enum.map(successes, fn {:ok, info} -> info end),
      errors: Enum.map(failures, fn {:error, info} -> info end)
    }}
  end

  defp validate_compiler(svc) do
    results = Enum.map(@compiler_modules, fn mod_name ->
      namespace = "Nova.Compiler.#{mod_name}"

      case Nova.NamespaceService.validate_namespace(svc, namespace) do
        {:ok, _} ->
          {:ok, %{module: mod_name, status: "valid"}}

        {:error, {:type_errors, errors}} ->
          {:error, %{module: mod_name, errors: errors}}

        {:error, reason} ->
          {:error, %{module: mod_name, error: inspect(reason)}}
      end
    end)

    successes = Enum.filter(results, fn {status, _} -> status == :ok end)
    failures = Enum.filter(results, fn {status, _} -> status == :error end)

    {:ok, %{
      valid: length(successes),
      invalid: length(failures),
      modules: Enum.map(successes, fn {:ok, info} -> info end),
      errors: Enum.map(failures, fn {:error, info} -> info end)
    }}
  end

  # ============================================================================
  # Multi-file Compilation
  # ============================================================================

  defp compile_project(svc, namespaces, output_dir, validate) do
    validate = if validate == nil, do: true, else: validate

    # Get list of namespaces to compile
    target_namespaces = case namespaces do
      nil ->
        {:ok, all} = Nova.NamespaceService.list_namespaces(svc)
        all
      list when is_list(list) -> list
    end

    if Enum.empty?(target_namespaces) do
      {:error, "No namespaces to compile"}
    else
      # Build dependency graph from imports
      dep_map = build_namespace_dependencies(svc, target_namespaces)

      # Topologically sort namespaces
      case topological_sort(target_namespaces, dep_map) do
        {:error, cycle} ->
          {:error, "Circular dependency detected: #{inspect(cycle)}"}

        {:ok, sorted_namespaces} ->
          # Optionally validate all namespaces first
          validation_results = if validate do
            Enum.map(sorted_namespaces, fn ns ->
              case Nova.NamespaceService.validate_namespace(svc, ns) do
                {:ok, _} -> {:ok, ns}
                {:error, {:type_errors, errors}} -> {:error, ns, errors}
                {:error, reason} -> {:error, ns, [inspect(reason)]}
              end
            end)
          else
            Enum.map(sorted_namespaces, fn ns -> {:ok, ns} end)
          end

          # Check for validation failures
          failures = Enum.filter(validation_results, fn
            {:error, _, _} -> true
            _ -> false
          end)

          if !Enum.empty?(failures) and validate do
            {:error, %{
              message: "Validation failed",
              errors: Enum.map(failures, fn {:error, ns, errs} ->
                %{namespace: ns, errors: errs}
              end)
            }}
          else
            # Create output directory
            File.mkdir_p!(output_dir)

            # Compile each namespace and write to file
            compile_results = Enum.map(sorted_namespaces, fn ns ->
              case compile_namespace(svc, ns) do
                {:ok, result} ->
                  # Convert namespace to file path
                  file_name = namespace_to_filename(ns)
                  file_path = Path.join(output_dir, file_name)

                  # Ensure subdirectories exist
                  File.mkdir_p!(Path.dirname(file_path))

                  case File.write(file_path, result.elixir_code) do
                    :ok ->
                      {:ok, %{
                        namespace: ns,
                        file: file_path,
                        lines: result.lines,
                        declarations: result.declarations
                      }}
                    {:error, reason} ->
                      {:error, %{namespace: ns, error: "Failed to write file: #{reason}"}}
                  end

                {:error, reason} ->
                  {:error, %{namespace: ns, error: inspect(reason)}}
              end
            end)

            successes = Enum.filter(compile_results, fn {status, _} -> status == :ok end)
            failures = Enum.filter(compile_results, fn {status, _} -> status == :error end)

            {:ok, %{
              output_dir: output_dir,
              compiled: length(successes),
              failed: length(failures),
              order: sorted_namespaces,
              files: Enum.map(successes, fn {:ok, info} -> info end),
              errors: Enum.map(failures, fn {:error, info} -> info end)
            }}
          end
      end
    end
  end

  defp build_namespace_dependencies(svc, namespaces) do
    namespace_set = MapSet.new(namespaces)

    Enum.reduce(namespaces, %{}, fn ns, acc ->
      imports = case Nova.NamespaceService.list_imports(svc, ns) do
        {:ok, imp_list} -> imp_list
        _ -> []
      end

      # Only include dependencies that are in our target set
      relevant_deps = Enum.filter(imports, fn imp -> MapSet.member?(namespace_set, imp) end)
      Map.put(acc, ns, relevant_deps)
    end)
  end

  defp topological_sort(namespaces, dep_map) do
    # Kahn's algorithm for topological sort
    # dep_map: namespace -> [namespaces it depends on (imports)]
    # We want to output in order where dependencies come BEFORE dependents
    # So a namespace with 0 dependencies can be compiled first

    # Calculate out-degrees (how many dependencies each node has)
    out_degrees = Enum.reduce(namespaces, %{}, fn ns, acc ->
      deps = Map.get(dep_map, ns, [])
      Map.put(acc, ns, length(deps))
    end)

    # Start with nodes that have no dependencies (out-degree 0)
    queue = Enum.filter(namespaces, fn ns -> Map.get(out_degrees, ns, 0) == 0 end)

    # Build reverse dependency map (who depends on each namespace)
    reverse_deps = Enum.reduce(dep_map, %{}, fn {ns, deps}, acc ->
      Enum.reduce(deps, acc, fn dep, acc2 ->
        Map.update(acc2, dep, [ns], fn existing -> [ns | existing] end)
      end)
    end)

    topological_sort_loop(queue, out_degrees, reverse_deps, [])
  end

  defp topological_sort_loop([], out_degrees, _reverse_deps, result) do
    # Check if all nodes processed
    remaining = Enum.filter(out_degrees, fn {_k, v} -> v > 0 end)
    if Enum.empty?(remaining) do
      {:ok, Enum.reverse(result)}
    else
      {:error, Enum.map(remaining, fn {k, _} -> k end)}
    end
  end

  defp topological_sort_loop([node | rest], out_degrees, reverse_deps, result) do
    # This node has no unprocessed dependencies, add to result
    # Now update nodes that depend on this one

    dependents = Map.get(reverse_deps, node, [])

    # Decrease out-degree for all dependents (they have one fewer unprocessed dependency)
    new_out_degrees = Enum.reduce(dependents, out_degrees, fn dep, acc ->
      new_deg = Map.get(acc, dep, 1) - 1
      Map.put(acc, dep, new_deg)
    end)

    # Remove processed node
    new_out_degrees = Map.delete(new_out_degrees, node)

    # Add newly available nodes (out-degree became 0, meaning all their dependencies are processed)
    new_available = Enum.filter(dependents, fn dep ->
      Map.get(new_out_degrees, dep, 0) == 0
    end)

    topological_sort_loop(rest ++ new_available, new_out_degrees, reverse_deps, [node | result])
  end

  defp namespace_to_filename(namespace) do
    # Convert "Nova.Compiler.Parser" to "nova/compiler/parser.ex"
    parts = String.split(namespace, ".")
    snake_parts = Enum.map(parts, &Macro.underscore/1)
    Path.join(snake_parts) <> ".ex"
  end

  # ============================================================================
  # Session Persistence
  # ============================================================================

  @default_session_dir ".nova_sessions"

  defp save_session(svc, name, description, directory) do
    dir = directory || @default_session_dir
    File.mkdir_p!(dir)

    case Nova.NamespaceService.export_state(svc) do
      {:ok, namespaces} ->
        session = %{
          version: 1,
          name: name,
          description: description,
          created_at: DateTime.utc_now() |> DateTime.to_iso8601(),
          namespaces: namespaces
        }

        path = Path.join(dir, "#{name}.json")
        case File.write(path, Jason.encode!(session, pretty: true)) do
          :ok ->
            total_decls = Enum.reduce(namespaces, 0, fn ns, acc ->
              acc + length(ns.declarations)
            end)
            {:ok, %{
              path: path,
              namespaces: length(namespaces),
              declarations: total_decls
            }}
          {:error, reason} ->
            {:error, "Failed to write session file: #{reason}"}
        end

      {:error, reason} ->
        {:error, "Failed to export state: #{inspect(reason)}"}
    end
  end

  defp load_session(svc, name, directory, merge) do
    dir = directory || @default_session_dir
    path = Path.join(dir, "#{name}.json")

    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, session} ->
            namespaces = session["namespaces"] || []

            case Nova.NamespaceService.import_state(svc, namespaces, merge || false) do
              {:ok, count} ->
                total_decls = Enum.reduce(namespaces, 0, fn ns, acc ->
                  acc + length(ns["declarations"] || [])
                end)
                {:ok, %{
                  name: session["name"],
                  description: session["description"],
                  created_at: session["created_at"],
                  namespaces_loaded: count,
                  declarations_loaded: total_decls,
                  merged: merge || false
                }}

              {:error, reason} ->
                {:error, "Failed to import session: #{inspect(reason)}"}
            end

          {:error, reason} ->
            {:error, "Failed to parse session file: #{inspect(reason)}"}
        end

      {:error, :enoent} ->
        {:error, "Session '#{name}' not found"}

      {:error, reason} ->
        {:error, "Failed to read session file: #{reason}"}
    end
  end

  defp list_sessions(directory) do
    dir = directory || @default_session_dir

    case File.ls(dir) do
      {:ok, files} ->
        sessions = files
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.map(fn file ->
          path = Path.join(dir, file)
          case File.read(path) do
            {:ok, content} ->
              case Jason.decode(content) do
                {:ok, session} ->
                  %{
                    name: session["name"],
                    description: session["description"],
                    created_at: session["created_at"],
                    namespaces: length(session["namespaces"] || []),
                    file: file
                  }
                _ -> nil
              end
            _ -> nil
          end
        end)
        |> Enum.reject(&is_nil/1)

        {:ok, sessions}

      {:error, :enoent} ->
        {:ok, []}

      {:error, reason} ->
        {:error, "Failed to list sessions: #{reason}"}
    end
  end

  defp delete_session(name, directory) do
    dir = directory || @default_session_dir
    path = Path.join(dir, "#{name}.json")

    case File.rm(path) do
      :ok ->
        {:ok, "Session '#{name}' deleted successfully"}

      {:error, :enoent} ->
        {:error, "Session '#{name}' not found"}

      {:error, reason} ->
        {:error, "Failed to delete session: #{reason}"}
    end
  end

  # ============================================================================
  # Expression Evaluation
  # ============================================================================

  defp eval_expression(svc, expression, namespace) do
    # Build context: collect functions from namespace if provided
    context_decls = if namespace do
      case Nova.NamespaceService.list_declarations(svc, namespace) do
        {:ok, decls} ->
          Enum.flat_map(decls, fn decl_info ->
            case Nova.NamespaceService.get_declaration(svc, namespace, decl_info.name) do
              {:ok, managed} -> [managed.decl]
              _ -> []
            end
          end)
        _ -> []
      end
    else
      []
    end

    # Parse the expression
    tokens = Nova.Compiler.Tokenizer.tokenize(expression)

    case Nova.Compiler.Parser.parse_expression(tokens) do
      {:left, err} ->
        {:error, "Parse error: #{inspect(err)}"}

      {:right, {:tuple, expr, _rest}} ->
        # Generate Elixir code for the expression
        elixir_code = Nova.Compiler.CodeGen.gen_expr_ctx(
          Nova.Compiler.CodeGen.empty_ctx(),
          0,
          expr
        )

        # Build bindings from context declarations
        bindings = build_eval_bindings(context_decls)

        # Evaluate the generated code
        try do
          {result, _bindings} = Code.eval_string(elixir_code, bindings, __ENV__)
          {:ok, %{
            expression: expression,
            elixir_code: elixir_code,
            result: format_eval_result(result)
          }}
        rescue
          e ->
            {:error, "Evaluation error: #{Exception.message(e)}"}
        catch
          :error, reason ->
            {:error, "Evaluation error: #{inspect(reason)}"}
        end
    end
  end

  defp build_eval_bindings(decls) do
    # For each function declaration, compile and bind it
    Enum.flat_map(decls, fn decl ->
      case decl do
        {:decl_function, func} ->
          # Generate Elixir code for the function
          ctx = Nova.Compiler.CodeGen.empty_ctx()
          code = Nova.Compiler.CodeGen.gen_function(ctx, func)
          try do
            # Create a module with the function
            module_code = """
            defmodule NovaEvalTemp#{:erlang.unique_integer([:positive])} do
              #{code}
            end
            """
            {{:module, mod, _, _}, _} = Code.eval_string(module_code)
            # Create curried wrapper for multi-arg functions
            # Nova codegen generates calls like f.(a).(b), so we need curried bindings
            arity = length(func.parameters)
            curried = curry_function(mod, String.to_atom(func.name), arity)
            [{String.to_atom(func.name), curried}]
          rescue
            _ -> []
          end
        _ -> []
      end
    end)
  end

  # Create a curried version of a function
  defp curry_function(mod, name, 0), do: apply(mod, name, [])
  defp curry_function(mod, name, 1), do: fn a -> apply(mod, name, [a]) end
  defp curry_function(mod, name, 2), do: fn a -> fn b -> apply(mod, name, [a, b]) end end
  defp curry_function(mod, name, 3), do: fn a -> fn b -> fn c -> apply(mod, name, [a, b, c]) end end end
  defp curry_function(mod, name, 4), do: fn a -> fn b -> fn c -> fn d -> apply(mod, name, [a, b, c, d]) end end end end
  defp curry_function(mod, name, arity), do: Function.capture(mod, name, arity)

  defp format_eval_result(result) when is_binary(result), do: inspect(result)
  defp format_eval_result(result) when is_list(result), do: inspect(result, charlists: :as_lists)
  defp format_eval_result(result), do: inspect(result)

  # ============================================================================
  # Undo/Redo
  # ============================================================================

  defp create_checkpoint(state, description) do
    svc = state.namespace_service

    # Export current state
    {:ok, namespaces} = Nova.NamespaceService.export_state(svc)

    checkpoint = %{
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      description: description || "Checkpoint",
      namespaces: namespaces,
      namespace_count: length(namespaces),
      declaration_count: Enum.reduce(namespaces, 0, fn ns, acc -> acc + length(ns.declarations) end)
    }

    # Add to undo stack, clear redo stack
    new_undo_stack = [checkpoint | state.undo_stack] |> Enum.take(state.max_history)
    new_state = %{state | undo_stack: new_undo_stack, redo_stack: []}

    {new_state, %{
      message: "Checkpoint created",
      description: checkpoint.description,
      namespaces: checkpoint.namespace_count,
      declarations: checkpoint.declaration_count,
      undo_stack_size: length(new_undo_stack)
    }}
  end

  defp undo(state) do
    case state.undo_stack do
      [] ->
        {:error, "Nothing to undo"}

      [checkpoint | rest] ->
        svc = state.namespace_service

        # Save current state to redo stack
        {:ok, current_namespaces} = Nova.NamespaceService.export_state(svc)
        current_checkpoint = %{
          timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
          description: "Before undo",
          namespaces: current_namespaces,
          namespace_count: length(current_namespaces),
          declaration_count: Enum.reduce(current_namespaces, 0, fn ns, acc -> acc + length(ns.declarations) end)
        }

        # Restore checkpoint state
        Nova.NamespaceService.import_state(svc, checkpoint.namespaces, false)

        new_state = %{state |
          undo_stack: rest,
          redo_stack: [current_checkpoint | state.redo_stack] |> Enum.take(state.max_history)
        }

        {:ok, new_state, %{
          message: "Undo successful",
          restored: checkpoint.description,
          restored_at: checkpoint.timestamp,
          namespaces: checkpoint.namespace_count,
          declarations: checkpoint.declaration_count,
          undo_remaining: length(rest),
          redo_available: length(new_state.redo_stack)
        }}
    end
  end

  defp redo(state) do
    case state.redo_stack do
      [] ->
        {:error, "Nothing to redo"}

      [checkpoint | rest] ->
        svc = state.namespace_service

        # Save current state to undo stack
        {:ok, current_namespaces} = Nova.NamespaceService.export_state(svc)
        current_checkpoint = %{
          timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
          description: "Before redo",
          namespaces: current_namespaces,
          namespace_count: length(current_namespaces),
          declaration_count: Enum.reduce(current_namespaces, 0, fn ns, acc -> acc + length(ns.declarations) end)
        }

        # Restore checkpoint state
        Nova.NamespaceService.import_state(svc, checkpoint.namespaces, false)

        new_state = %{state |
          undo_stack: [current_checkpoint | state.undo_stack] |> Enum.take(state.max_history),
          redo_stack: rest
        }

        {:ok, new_state, %{
          message: "Redo successful",
          restored: checkpoint.description,
          namespaces: checkpoint.namespace_count,
          declarations: checkpoint.declaration_count,
          undo_available: length(new_state.undo_stack),
          redo_remaining: length(rest)
        }}
    end
  end

  defp list_checkpoints(state) do
    undo_list = Enum.with_index(state.undo_stack) |> Enum.map(fn {cp, idx} ->
      %{
        index: idx,
        type: "undo",
        description: cp.description,
        timestamp: cp.timestamp,
        namespaces: cp.namespace_count,
        declarations: cp.declaration_count
      }
    end)

    redo_list = Enum.with_index(state.redo_stack) |> Enum.map(fn {cp, idx} ->
      %{
        index: idx,
        type: "redo",
        description: cp.description,
        timestamp: cp.timestamp,
        namespaces: cp.namespace_count,
        declarations: cp.declaration_count
      }
    end)

    %{
      undo_stack: undo_list,
      redo_stack: redo_list,
      undo_count: length(state.undo_stack),
      redo_count: length(state.redo_stack)
    }
  end

  # ============================================================================
  # Testing
  # ============================================================================

  defp run_tests(svc, namespace, pattern) do
    prefix = pattern || "test_"

    case Nova.NamespaceService.list_declarations(svc, namespace) do
      {:ok, decls} ->
        # Find test functions
        test_decls = Enum.filter(decls, fn d ->
          d.kind == :function && String.starts_with?(d.name, prefix)
        end)

        if Enum.empty?(test_decls) do
          {:ok, %{
            namespace: namespace,
            pattern: prefix,
            message: "No tests found",
            total: 0,
            passed: 0,
            failed: 0,
            results: []
          }}
        else
          # Run each test
          results = Enum.map(test_decls, fn test_decl ->
            run_single_test(svc, namespace, test_decl.name)
          end)

          passed = Enum.count(results, fn r -> r.status == :passed end)
          failed = Enum.count(results, fn r -> r.status == :failed end)

          {:ok, %{
            namespace: namespace,
            pattern: prefix,
            total: length(results),
            passed: passed,
            failed: failed,
            results: results
          }}
        end

      {:error, reason} ->
        {:error, "Failed to list declarations: #{inspect(reason)}"}
    end
  end

  defp run_single_test(svc, namespace, test_name) do
    # Get the test declaration and evaluate its body expression directly
    case Nova.NamespaceService.get_declaration(svc, namespace, test_name) do
      {:ok, managed} ->
        # Extract the body expression from the function
        case managed.decl do
          {:decl_function, func} ->
            # Evaluate the function body directly with all namespace bindings
            body = func.body
            elixir_code = Nova.Compiler.CodeGen.gen_expr_ctx(
              Nova.Compiler.CodeGen.empty_ctx(),
              0,
              body
            )

            # Get all declarations for bindings
            context_decls = case Nova.NamespaceService.list_declarations(svc, namespace) do
              {:ok, decls} ->
                Enum.flat_map(decls, fn decl_info ->
                  case Nova.NamespaceService.get_declaration(svc, namespace, decl_info.name) do
                    {:ok, m} -> [m.decl]
                    _ -> []
                  end
                end)
              _ -> []
            end

            bindings = build_eval_bindings(context_decls)

            try do
              {result, _} = Code.eval_string(elixir_code, bindings, __ENV__)
              case result do
                true -> %{name: test_name, status: :passed, result: "true"}
                false -> %{name: test_name, status: :failed, result: "false", error: "Test returned false"}
                other -> %{name: test_name, status: :passed, result: inspect(other)}
              end
            rescue
              e -> %{name: test_name, status: :failed, error: Exception.message(e)}
            end

          _ ->
            %{name: test_name, status: :failed, error: "Not a function declaration"}
        end

      {:error, reason} ->
        %{name: test_name, status: :failed, error: inspect(reason)}
    end
  end

  defp run_assert(svc, expression, expected, namespace, description) do
    case eval_expression_internal(svc, expression, namespace) do
      {:ok, result, elixir_code} ->
        result_str = format_eval_result(result)
        # Compare string representations
        passed = result_str == expected || inspect(result) == expected

        if passed do
          {:ok, %{
            status: :passed,
            expression: expression,
            expected: expected,
            actual: result_str,
            description: description
          }}
        else
          {:ok, %{
            status: :failed,
            expression: expression,
            expected: expected,
            actual: result_str,
            elixir_code: elixir_code,
            description: description
          }}
        end

      {:error, reason} ->
        {:ok, %{
          status: :error,
          expression: expression,
          expected: expected,
          error: reason,
          description: description
        }}
    end
  end

  # Internal eval that returns raw result
  defp eval_expression_internal(svc, expression, namespace) do
    context_decls = if namespace do
      case Nova.NamespaceService.list_declarations(svc, namespace) do
        {:ok, decls} ->
          Enum.flat_map(decls, fn decl_info ->
            case Nova.NamespaceService.get_declaration(svc, namespace, decl_info.name) do
              {:ok, managed} -> [managed.decl]
              _ -> []
            end
          end)
        _ -> []
      end
    else
      []
    end

    tokens = Nova.Compiler.Tokenizer.tokenize(expression)

    case Nova.Compiler.Parser.parse_expression(tokens) do
      {:left, err} ->
        {:error, "Parse error: #{inspect(err)}"}

      {:right, {:tuple, expr, _rest}} ->
        elixir_code = Nova.Compiler.CodeGen.gen_expr_ctx(
          Nova.Compiler.CodeGen.empty_ctx(),
          0,
          expr
        )

        bindings = build_eval_bindings(context_decls)

        try do
          {result, _bindings} = Code.eval_string(elixir_code, bindings, __ENV__)
          {:ok, result, elixir_code}
        rescue
          e ->
            {:error, "Evaluation error: #{Exception.message(e)}"}
        end
    end
  end

  # ============================================================================
  # Expression Type Inference
  # ============================================================================

  defp get_expression_type(svc, expression, namespace) do
    # Build type environment from namespace context
    env = build_type_env(svc, namespace)

    # Parse the expression
    tokens = Nova.Compiler.Tokenizer.tokenize(expression)

    case Nova.Compiler.Parser.parse_expression(tokens) do
      {:left, err} ->
        {:error, "Parse error: #{inspect(err)}"}

      {:right, {:tuple, expr, _rest}} ->
        # Run type inference
        case Nova.Compiler.TypeChecker.infer(env, expr) do
          {:right, result} ->
            # Apply substitution to get final type
            final_type = Nova.Compiler.Types.apply_subst(result.sub, result.ty)
            {:ok, %{
              expression: expression,
              type: format_type(final_type),
              raw_type: inspect_type(final_type)
            }}

          {:left, err} ->
            {:error, "Type error: #{format_type_error(err)}"}
        end
    end
  end

  defp build_type_env(svc, namespace) do
    base_env = Nova.Compiler.Types.empty_env()

    if namespace do
      # Get declarations from the namespace and add their types to the env
      case Nova.NamespaceService.list_declarations(svc, namespace) do
        {:ok, decls} ->
          Enum.reduce(decls, base_env, fn decl_info, env ->
            case Nova.NamespaceService.get_declaration(svc, namespace, decl_info.name) do
              {:ok, managed} ->
                # If we have type info, add it to the environment
                case managed.type_info do
                  %{scheme: scheme} when not is_nil(scheme) ->
                    Nova.Compiler.Types.extend_env(env, decl_info.name, scheme)
                  _ ->
                    env
                end
              _ ->
                env
            end
          end)
        _ ->
          base_env
      end
    else
      base_env
    end
  end

  defp inspect_type(type) do
    # Return a more detailed internal representation
    inspect(type)
  end

  defp format_type_error({:unbound_variable, name}) do
    "Unbound variable: #{name}"
  end

  defp format_type_error({:unify_err, {:type_mismatch, t1, t2}}) do
    "Type mismatch: expected #{format_type(t1)}, got #{format_type(t2)}"
  end

  defp format_type_error({:unify_err, {:occurs_check, var, ty}}) do
    "Infinite type: #{inspect(var)} occurs in #{format_type(ty)}"
  end

  defp format_type_error(err) do
    inspect(err)
  end
end
