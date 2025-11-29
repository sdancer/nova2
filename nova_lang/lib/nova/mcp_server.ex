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
    {:ok, %{namespace_service: svc}}
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

      _ ->
        {:error, "Unknown tool: #{name}"}
    end

    case result do
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
  defp format_type(type), do: Nova.Compiler.Unify.show_type(type)

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
end
