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
end
