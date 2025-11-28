defmodule Nova.MCP.Server do
  @moduledoc """
  MCP (Model Context Protocol) Server for Nova Compiler.

  This server exposes Nova's compilation and namespace management features
  through the MCP protocol, allowing AI assistants to interact with the
  Nova compiler programmatically.

  ## Starting the Server

      Nova.MCP.Server.start()

  Or via CLI:

      mix run --no-halt -e "Nova.MCP.Server.start()"

  ## Available Tools

  ### Namespace Management
  - `nova_create_namespace` - Create a new namespace
  - `nova_delete_namespace` - Delete a namespace
  - `nova_list_namespaces` - List all namespaces

  ### Declaration Management
  - `nova_add_declaration` - Add code to a namespace
  - `nova_update_declaration` - Update existing declaration
  - `nova_remove_declaration` - Remove a declaration
  - `nova_list_declarations` - List declarations in namespace
  - `nova_get_declaration` - Get declaration by name

  ### Type & Validation
  - `nova_validate` - Type-check a namespace
  - `nova_get_type` - Get inferred type for declaration
  - `nova_get_diagnostics` - Get errors for namespace
  - `nova_get_completions` - Get completions for prefix

  ### File Operations
  - `nova_import_file` - Load Nova file into namespace
  - `nova_read_source` - Read a declaration's source

  ### Compilation
  - `nova_compile` - Compile source string to Elixir
  - `nova_compile_namespace` - Compile validated namespace to Elixir
  """

  require Logger

  @doc """
  Start the MCP server with stdio transport.
  """
  def start do
    Logger.info("Starting Nova MCP Server...")

    # Start the NamespaceService
    {:ok, svc} = Nova.NamespaceService.start_link(name: Nova.MCP.NamespaceService)

    # Start MCP server with stdio transport
    case Code.ensure_loaded(ExMCP.Server) do
      {:module, _} ->
        ExMCP.Server.start_link(
          handler: Nova.MCP.Handler,
          handler_state: %{namespace_service: svc},
          transport: :stdio
        )

      {:error, _} ->
        # Fallback: run our own simple stdio server
        Logger.info("ExMCP not available, using built-in stdio server")
        run_stdio_server(svc)
    end
  end

  @doc """
  Run a simple stdio-based MCP server (fallback if ex_mcp not available).
  """
  def run_stdio_server(namespace_service) do
    state = %{namespace_service: namespace_service, initialized: false}
    stdio_loop(state)
  end

  defp stdio_loop(state) do
    case IO.read(:stdio, :line) do
      :eof ->
        Logger.info("MCP Server: EOF received, shutting down")
        :ok

      {:error, reason} ->
        Logger.error("MCP Server: Read error: #{inspect(reason)}")
        :error

      line when is_binary(line) ->
        line = String.trim(line)
        if line != "" do
          case Jason.decode(line) do
            {:ok, request} ->
              {response, new_state} = handle_jsonrpc(request, state)
              if response do
                IO.puts(Jason.encode!(response))
              end
              stdio_loop(new_state)

            {:error, _} ->
              error_response = %{
                "jsonrpc" => "2.0",
                "error" => %{"code" => -32700, "message" => "Parse error"},
                "id" => nil
              }
              IO.puts(Jason.encode!(error_response))
              stdio_loop(state)
          end
        else
          stdio_loop(state)
        end
    end
  end

  defp handle_jsonrpc(%{"method" => "initialize", "id" => id} = _request, state) do
    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{
        "protocolVersion" => "2024-11-05",
        "serverInfo" => %{
          "name" => "nova-compiler",
          "version" => "0.1.0"
        },
        "capabilities" => %{
          "tools" => %{}
        }
      }
    }
    {response, %{state | initialized: true}}
  end

  defp handle_jsonrpc(%{"method" => "notifications/initialized"}, state) do
    {nil, state}
  end

  defp handle_jsonrpc(%{"method" => "tools/list", "id" => id}, state) do
    tools = Nova.MCP.Tools.list_tools()
    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{"tools" => tools}
    }
    {response, state}
  end

  defp handle_jsonrpc(%{"method" => "tools/call", "id" => id, "params" => params}, state) do
    tool_name = params["name"]
    arguments = params["arguments"] || %{}

    result = Nova.MCP.Tools.call_tool(tool_name, arguments, state.namespace_service)

    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => result
    }
    {response, state}
  end

  defp handle_jsonrpc(%{"method" => method, "id" => id}, state) do
    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "error" => %{
        "code" => -32601,
        "message" => "Method not found: #{method}"
      }
    }
    {response, state}
  end

  defp handle_jsonrpc(%{"method" => _method}, state) do
    # Notification without id - no response needed
    {nil, state}
  end
end
