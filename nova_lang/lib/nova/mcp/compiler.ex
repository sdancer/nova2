defmodule Nova.MCP.Compiler do
  @moduledoc """
  Compilation utilities for the Nova MCP server.

  Handles compiling namespaces to Elixir code.
  """

  @doc """
  Compile all declarations in a namespace to Elixir code.

  The namespace must be validated first (all declarations in :valid status).

  ## Parameters
  - `server` - The NamespaceService process
  - `namespace` - The namespace to compile

  ## Returns
  - `{:ok, elixir_code}` - The generated Elixir code
  - `{:error, reason}` - If compilation fails
  """
  def compile_namespace(server, namespace) do
    # Get all declarations
    case Nova.NamespaceService.list_declarations(server, namespace) do
      {:error, reason} ->
        {:error, reason}

      {:ok, decls} ->
        # Check all are validated
        invalid = Enum.filter(decls, fn d -> d.status != :valid end)

        if invalid != [] do
          names = Enum.map(invalid, fn d -> d.name end) |> Enum.join(", ")
          {:error, {:not_validated, "Declarations not validated: #{names}"}}
        else
          # Get full declaration data for code generation
          results = Enum.map(decls, fn d ->
            Nova.NamespaceService.get_declaration(server, namespace, d.name)
          end)

          # Collect ASTs
          asts = results
          |> Enum.filter(fn
            {:ok, _} -> true
            _ -> false
          end)
          |> Enum.map(fn {:ok, managed} -> managed.decl end)

          # Build a pseudo-module for code generation
          mod = %{
            name: namespace_to_module_name(namespace),
            declarations: asts
          }

          # Generate code
          elixir_code = Nova.Compiler.CodeGen.gen_module().(mod)
          {:ok, elixir_code}
        end
    end
  end

  @doc """
  Compile multiple namespaces to a single Elixir file.

  Useful for compiling a whole project.
  """
  def compile_namespaces(server, namespaces) do
    results = Enum.map(namespaces, fn ns ->
      case compile_namespace(server, ns) do
        {:ok, code} -> {:ok, ns, code}
        {:error, reason} -> {:error, ns, reason}
      end
    end)

    errors = Enum.filter(results, fn
      {:error, _, _} -> true
      _ -> false
    end)

    if errors == [] do
      code = results
      |> Enum.map(fn {:ok, _ns, code} -> code end)
      |> Enum.join("\n\n")
      {:ok, code}
    else
      error_msgs = Enum.map(errors, fn {:error, ns, reason} ->
        "#{ns}: #{inspect(reason)}"
      end)
      {:error, {:compile_errors, error_msgs}}
    end
  end

  @doc """
  Compile a namespace and write to a file.
  """
  def compile_namespace_to_file(server, namespace, output_path) do
    case compile_namespace(server, namespace) do
      {:ok, code} ->
        File.write(output_path, code)

      error ->
        error
    end
  end

  # Convert namespace name to Elixir module name
  defp namespace_to_module_name(namespace) do
    # Nova namespaces are already in dot notation
    # Just ensure proper capitalization
    namespace
    |> String.split(".")
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(".")
  end
end
