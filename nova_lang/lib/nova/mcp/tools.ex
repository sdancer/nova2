defmodule Nova.MCP.Tools do
  @moduledoc """
  MCP Tool definitions and handlers for Nova Compiler.
  """

  @doc """
  Returns the list of all available MCP tools.
  """
  def list_tools do
    [
      # Namespace Management
      %{
        "name" => "nova_create_namespace",
        "description" => "Create a new namespace for organizing Nova declarations. Namespaces are containers for functions, data types, and other declarations.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "name" => %{
              "type" => "string",
              "description" => "The namespace name (e.g., 'MyApp.Utils', 'Math.Core')"
            }
          },
          "required" => ["name"]
        }
      },
      %{
        "name" => "nova_delete_namespace",
        "description" => "Delete a namespace and all its declarations.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "name" => %{
              "type" => "string",
              "description" => "The namespace name to delete"
            }
          },
          "required" => ["name"]
        }
      },
      %{
        "name" => "nova_list_namespaces",
        "description" => "List all existing namespaces.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{}
        }
      },
      %{
        "name" => "nova_add_import",
        "description" => "Add an import from one namespace to another, making declarations from the imported namespace available.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "namespace" => %{
              "type" => "string",
              "description" => "The namespace to add the import to"
            },
            "import" => %{
              "type" => "string",
              "description" => "The namespace to import from"
            }
          },
          "required" => ["namespace", "import"]
        }
      },
      %{
        "name" => "nova_list_imports",
        "description" => "List all imports for a namespace.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "namespace" => %{
              "type" => "string",
              "description" => "The namespace to list imports for"
            }
          },
          "required" => ["namespace"]
        }
      },

      # Declaration Management
      %{
        "name" => "nova_add_declaration",
        "description" => "Add a Nova declaration (function, data type, type class, etc.) to a namespace. Returns the declaration ID.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "namespace" => %{
              "type" => "string",
              "description" => "The namespace to add the declaration to"
            },
            "source" => %{
              "type" => "string",
              "description" => "The Nova source code for the declaration (e.g., 'add x y = x + y')"
            }
          },
          "required" => ["namespace", "source"]
        }
      },
      %{
        "name" => "nova_update_declaration",
        "description" => "Update an existing declaration with new source code.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "decl_id" => %{
              "type" => "string",
              "description" => "The declaration ID to update"
            },
            "source" => %{
              "type" => "string",
              "description" => "The new Nova source code"
            }
          },
          "required" => ["decl_id", "source"]
        }
      },
      %{
        "name" => "nova_remove_declaration",
        "description" => "Remove a declaration from its namespace.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "decl_id" => %{
              "type" => "string",
              "description" => "The declaration ID to remove"
            }
          },
          "required" => ["decl_id"]
        }
      },
      %{
        "name" => "nova_list_declarations",
        "description" => "List all declarations in a namespace with their IDs, names, kinds, and status.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "namespace" => %{
              "type" => "string",
              "description" => "The namespace to list declarations from"
            }
          },
          "required" => ["namespace"]
        }
      },
      %{
        "name" => "nova_get_declaration",
        "description" => "Get detailed information about a declaration by name, including its source code.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "namespace" => %{
              "type" => "string",
              "description" => "The namespace containing the declaration"
            },
            "name" => %{
              "type" => "string",
              "description" => "The declaration name"
            }
          },
          "required" => ["namespace", "name"]
        }
      },

      # Type & Validation
      %{
        "name" => "nova_validate",
        "description" => "Type-check all declarations in a namespace. Returns the count of validated declarations and any errors.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "namespace" => %{
              "type" => "string",
              "description" => "The namespace to validate"
            }
          },
          "required" => ["namespace"]
        }
      },
      %{
        "name" => "nova_get_type",
        "description" => "Get the inferred type of a declaration (must be validated first).",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "decl_id" => %{
              "type" => "string",
              "description" => "The declaration ID"
            }
          },
          "required" => ["decl_id"]
        }
      },
      %{
        "name" => "nova_get_diagnostics",
        "description" => "Get all diagnostic messages (errors, warnings) for a namespace.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "namespace" => %{
              "type" => "string",
              "description" => "The namespace to get diagnostics for"
            }
          },
          "required" => ["namespace"]
        }
      },
      %{
        "name" => "nova_get_completions",
        "description" => "Get code completions for a prefix in a namespace.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "namespace" => %{
              "type" => "string",
              "description" => "The namespace to get completions from"
            },
            "prefix" => %{
              "type" => "string",
              "description" => "The prefix to complete"
            }
          },
          "required" => ["namespace", "prefix"]
        }
      },

      # File Operations
      %{
        "name" => "nova_import_file",
        "description" => "Import a Nova source file into a namespace. Parses the file and adds all declarations to the namespace.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "path" => %{
              "type" => "string",
              "description" => "Path to the Nova source file"
            },
            "namespace" => %{
              "type" => "string",
              "description" => "The namespace to import declarations into"
            }
          },
          "required" => ["path", "namespace"]
        }
      },
      %{
        "name" => "nova_import_directory",
        "description" => "Import all .nova files from a directory into namespaces. File paths determine namespace names.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "path" => %{
              "type" => "string",
              "description" => "Path to the directory containing Nova files"
            },
            "namespace_prefix" => %{
              "type" => "string",
              "description" => "Prefix for generated namespace names (e.g., 'MyApp')"
            }
          },
          "required" => ["path", "namespace_prefix"]
        }
      },

      # Compilation
      %{
        "name" => "nova_compile",
        "description" => "Compile Nova source code to Elixir. Includes tokenization, parsing, type checking, and code generation.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "source" => %{
              "type" => "string",
              "description" => "Nova source code to compile"
            }
          },
          "required" => ["source"]
        }
      },
      %{
        "name" => "nova_compile_namespace",
        "description" => "Compile all declarations in a validated namespace to Elixir code.",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "namespace" => %{
              "type" => "string",
              "description" => "The namespace to compile"
            }
          },
          "required" => ["namespace"]
        }
      },
      %{
        "name" => "nova_parse",
        "description" => "Parse Nova source code and return the AST (for debugging/inspection).",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "source" => %{
              "type" => "string",
              "description" => "Nova source code to parse"
            }
          },
          "required" => ["source"]
        }
      }
    ]
  end

  @doc """
  Call a tool by name with the given arguments.
  """
  def call_tool(name, args, namespace_service) do
    try do
      result = do_call_tool(name, args, namespace_service)
      format_tool_result(result)
    rescue
      e ->
        %{
          "content" => [%{"type" => "text", "text" => "Error: #{Exception.message(e)}"}],
          "isError" => true
        }
    end
  end

  # Namespace Management

  defp do_call_tool("nova_create_namespace", %{"name" => name}, svc) do
    case Nova.NamespaceService.create_namespace(svc, name) do
      :ok -> {:ok, "Created namespace: #{name}"}
      {:error, :already_exists} -> {:error, "Namespace '#{name}' already exists"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_delete_namespace", %{"name" => name}, svc) do
    case Nova.NamespaceService.delete_namespace(svc, name) do
      :ok -> {:ok, "Deleted namespace: #{name}"}
      {:error, :not_found} -> {:error, "Namespace '#{name}' not found"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_list_namespaces", _args, svc) do
    case Nova.NamespaceService.list_namespaces(svc) do
      {:ok, namespaces} ->
        if namespaces == [] do
          {:ok, "No namespaces exist yet."}
        else
          {:ok, "Namespaces:\n" <> Enum.join(namespaces, "\n")}
        end
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_add_import", %{"namespace" => ns, "import" => imp}, svc) do
    case Nova.NamespaceService.add_import(svc, ns, imp) do
      :ok -> {:ok, "Added import '#{imp}' to namespace '#{ns}'"}
      {:ok, :already_imported} -> {:ok, "Import '#{imp}' already exists in namespace '#{ns}'"}
      {:error, :namespace_not_found} -> {:error, "Namespace '#{ns}' not found"}
      {:error, {:import_not_found, _}} -> {:error, "Import namespace '#{imp}' not found"}
      {:error, {:circular_import, _, _}} -> {:error, "Circular import detected"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_list_imports", %{"namespace" => ns}, svc) do
    case Nova.NamespaceService.list_imports(svc, ns) do
      {:ok, imports} ->
        if imports == [] do
          {:ok, "Namespace '#{ns}' has no imports."}
        else
          {:ok, "Imports for '#{ns}':\n" <> Enum.join(imports, "\n")}
        end
      {:error, :namespace_not_found} -> {:error, "Namespace '#{ns}' not found"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  # Declaration Management

  defp do_call_tool("nova_add_declaration", %{"namespace" => ns, "source" => source}, svc) do
    case Nova.NamespaceService.add_declaration(svc, ns, source) do
      {:ok, decl_id} -> {:ok, "Added declaration with ID: #{decl_id}"}
      {:error, :namespace_not_found} -> {:error, "Namespace '#{ns}' not found"}
      {:error, {:parse_error, err}} -> {:error, "Parse error: #{err}"}
      {:error, {:duplicate_name, name}} -> {:error, "Duplicate name: #{name}"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_update_declaration", %{"decl_id" => id, "source" => source}, svc) do
    case Nova.NamespaceService.update_declaration(svc, id, source) do
      {:ok, _} -> {:ok, "Updated declaration: #{id}"}
      {:error, :not_found} -> {:error, "Declaration '#{id}' not found"}
      {:error, {:parse_error, err}} -> {:error, "Parse error: #{err}"}
      {:error, {:name_mismatch, old, new}} -> {:error, "Name mismatch: expected '#{old}', got '#{new}'"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_remove_declaration", %{"decl_id" => id}, svc) do
    case Nova.NamespaceService.remove_declaration(svc, id) do
      :ok -> {:ok, "Removed declaration: #{id}"}
      {:error, :not_found} -> {:error, "Declaration '#{id}' not found"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_list_declarations", %{"namespace" => ns}, svc) do
    case Nova.NamespaceService.list_declarations(svc, ns) do
      {:ok, decls} ->
        if decls == [] do
          {:ok, "Namespace '#{ns}' has no declarations."}
        else
          text = decls
          |> Enum.map(fn d ->
            "- #{d.name} (#{d.kind}) [#{d.status}] ID: #{d.decl_id}"
          end)
          |> Enum.join("\n")
          {:ok, "Declarations in '#{ns}':\n#{text}"}
        end
      {:error, :namespace_not_found} -> {:error, "Namespace '#{ns}' not found"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_get_declaration", %{"namespace" => ns, "name" => name}, svc) do
    case Nova.NamespaceService.get_declaration(svc, ns, name) do
      {:ok, managed} ->
        text = """
        Declaration: #{managed.meta.name}
        Kind: #{managed.meta.kind}
        Status: #{managed.meta.status}
        Version: #{managed.meta.version}
        ID: #{managed.meta.decl_id}

        Source:
        ```nova
        #{managed.source_text}
        ```
        """
        {:ok, String.trim(text)}
      {:error, :namespace_not_found} -> {:error, "Namespace '#{ns}' not found"}
      {:error, :not_found} -> {:error, "Declaration '#{name}' not found in namespace '#{ns}'"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  # Type & Validation

  defp do_call_tool("nova_validate", %{"namespace" => ns}, svc) do
    case Nova.NamespaceService.validate_namespace(svc, ns) do
      {:ok, count} -> {:ok, "Validated #{count} declaration(s) in namespace '#{ns}'"}
      {:error, :namespace_not_found} -> {:error, "Namespace '#{ns}' not found"}
      {:error, {:type_errors, errors}} ->
        error_text = errors |> Enum.join("\n- ")
        {:error, "Type errors:\n- #{error_text}"}
      {:error, {:import_validation_failed, reason}} ->
        {:error, "Import validation failed: #{inspect(reason)}"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_get_type", %{"decl_id" => id}, svc) do
    case Nova.NamespaceService.get_type(svc, id) do
      {:ok, type} -> {:ok, "Type: #{format_type(type)}"}
      {:error, :not_found} -> {:error, "Declaration '#{id}' not found"}
      {:error, {:type_errors, errors}} -> {:error, "Type errors: #{inspect(errors)}"}
      {:error, {:not_validated, status}} -> {:error, "Declaration not validated (status: #{status}). Run nova_validate first."}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_get_diagnostics", %{"namespace" => ns}, svc) do
    case Nova.NamespaceService.get_diagnostics(svc, ns) do
      {:ok, diagnostics} ->
        if diagnostics == [] do
          {:ok, "No diagnostics for namespace '#{ns}'."}
        else
          text = diagnostics
          |> Enum.map(fn d -> "- [#{d.severity}] #{d.name}: #{d.message}" end)
          |> Enum.join("\n")
          {:ok, "Diagnostics for '#{ns}':\n#{text}"}
        end
      {:error, :namespace_not_found} -> {:error, "Namespace '#{ns}' not found"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_get_completions", %{"namespace" => ns, "prefix" => prefix}, svc) do
    case Nova.NamespaceService.get_completions(svc, ns, prefix) do
      {:ok, completions} ->
        if completions == [] do
          {:ok, "No completions for prefix '#{prefix}' in namespace '#{ns}'."}
        else
          text = completions
          |> Enum.map(fn c ->
            type_str = if c.type, do: " :: #{format_type(c.type)}", else: ""
            "- #{c.name} (#{c.kind})#{type_str}"
          end)
          |> Enum.join("\n")
          {:ok, "Completions for '#{prefix}':\n#{text}"}
        end
      {:error, :namespace_not_found} -> {:error, "Namespace '#{ns}' not found"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  # File Operations

  defp do_call_tool("nova_import_file", %{"path" => path, "namespace" => ns}, svc) do
    case Nova.MCP.FileImporter.import_file(svc, path, ns) do
      {:ok, results} ->
        success_count = Enum.count(results, fn {status, _} -> status == :ok end)
        {:ok, "Imported #{success_count} declaration(s) from '#{path}' into namespace '#{ns}'"}
      {:error, :file_not_found} -> {:error, "File not found: #{path}"}
      {:error, {:parse_error, err}} -> {:error, "Parse error: #{err}"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_import_directory", %{"path" => path, "namespace_prefix" => prefix}, svc) do
    case Nova.MCP.FileImporter.import_directory(svc, path, prefix) do
      {:ok, results} ->
        text = results
        |> Enum.map(fn {file, count} -> "- #{file}: #{count} declaration(s)" end)
        |> Enum.join("\n")
        {:ok, "Imported files:\n#{text}"}
      {:error, :directory_not_found} -> {:error, "Directory not found: #{path}"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  # Compilation

  defp do_call_tool("nova_compile", %{"source" => source}, _svc) do
    case Nova.compile(source) do
      {:ok, elixir_code} ->
        {:ok, "Generated Elixir code:\n```elixir\n#{elixir_code}\n```"}
      {:error, {:parse, err}} -> {:error, "Parse error: #{err}"}
      {:error, {:typecheck, err}} -> {:error, "Type error: #{inspect(err)}"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_compile_namespace", %{"namespace" => ns}, svc) do
    case Nova.MCP.Compiler.compile_namespace(svc, ns) do
      {:ok, elixir_code} ->
        {:ok, "Generated Elixir code for namespace '#{ns}':\n```elixir\n#{elixir_code}\n```"}
      {:error, :namespace_not_found} -> {:error, "Namespace '#{ns}' not found"}
      {:error, {:not_validated, _}} -> {:error, "Namespace not validated. Run nova_validate first."}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp do_call_tool("nova_parse", %{"source" => source}, _svc) do
    tokens = Nova.Compiler.Tokenizer.tokenize(source)
    case Nova.Compiler.Parser.parse_module(tokens) do
      {:right, {:tuple, mod, _rest}} ->
        ast_str = inspect(mod, pretty: true, limit: :infinity)
        {:ok, "Parsed AST:\n```\n#{ast_str}\n```"}
      {:left, err} ->
        {:error, "Parse error: #{err}"}
    end
  end

  defp do_call_tool(name, _args, _svc) do
    {:error, "Unknown tool: #{name}"}
  end

  # Helper functions

  defp format_tool_result({:ok, text}) do
    %{
      "content" => [%{"type" => "text", "text" => text}],
      "isError" => false
    }
  end

  defp format_tool_result({:error, text}) do
    %{
      "content" => [%{"type" => "text", "text" => text}],
      "isError" => true
    }
  end

  defp format_type(nil), do: "unknown"
  defp format_type({:forall, vars, ty}) do
    var_str = Enum.join(vars, " ")
    "forall #{var_str}. #{format_type(ty)}"
  end
  defp format_type({:ty_con, %{name: name, args: []}}), do: name
  defp format_type({:ty_con, %{name: name, args: args}}) do
    arg_strs = Enum.map(args, &format_type/1) |> Enum.join(" ")
    "(#{name} #{arg_strs})"
  end
  defp format_type({:ty_var, %{name: name}}), do: name
  defp format_type({:ty_arrow, from, to}), do: "(#{format_type(from)} -> #{format_type(to)})"
  defp format_type({:ty_record, %{fields: fields}}) do
    field_strs = Enum.map(fields, fn {k, v} -> "#{k} :: #{format_type(v)}" end) |> Enum.join(", ")
    "{ #{field_strs} }"
  end
  defp format_type(other), do: inspect(other)
end
