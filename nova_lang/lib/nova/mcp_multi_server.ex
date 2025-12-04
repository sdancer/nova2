defmodule Nova.MCPMultiServer do
  @moduledoc """
  Multi-connection MCP server for Nova.

  Listens on a TCP port and spawns a new MCP server instance for each
  incoming connection.

  ## Modes

  - **Isolated mode** (default): Each client gets their own isolated namespace service.
    Changes made by one client are not visible to other clients.

  - **Shared mode**: All clients share the same namespace service.
    Changes made by one client are immediately visible to all other clients.
    Use `shared: true` option to enable.

  ## Usage

      # Isolated mode (default) - each client has separate state
      {:ok, pid} = Nova.MCPMultiServer.start_link(port: 9999)

      # Shared mode - all clients share state
      {:ok, pid} = Nova.MCPMultiServer.start_link(port: 9999, shared: true)

  Or via CLI:

      nova mcp --port 9999             # isolated mode
      nova mcp --port 9999 --shared    # shared mode
  """

  use GenServer
  require Logger

  defstruct [:listen_socket, :port, :connections, :shared, :shared_namespace_service]

  @default_port 9999

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    port = Keyword.get(opts, :port, @default_port)
    shared = Keyword.get(opts, :shared, false)

    # For shared mode, create a single namespace service for all connections
    shared_svc = if shared do
      {:ok, svc} = Nova.NamespaceService.start_link()
      svc
    else
      nil
    end

    case :gen_tcp.listen(port, [
      :binary,
      packet: :line,
      active: false,
      reuseaddr: true
    ]) do
      {:ok, listen_socket} ->
        mode_str = if shared, do: "shared", else: "isolated"
        IO.puts(:stderr, "Nova MCP server listening on port #{port} (#{mode_str} mode)")
        IO.puts(:stderr, "Accepting multiple connections...")

        state = %__MODULE__{
          listen_socket: listen_socket,
          port: port,
          connections: %{},
          shared: shared,
          shared_namespace_service: shared_svc
        }

        # Start accepting connections
        send(self(), :accept)

        {:ok, state}

      {:error, reason} ->
        {:stop, {:listen_failed, reason}}
    end
  end

  @impl true
  def handle_info(:accept, state) do
    # Accept in a spawned process to not block the GenServer
    parent = self()
    spawn_link(fn -> accept_loop(parent, state.listen_socket) end)
    {:noreply, state}
  end

  def handle_info({:new_connection, client_socket}, state) do
    # Spawn a new connection handler for this client
    conn_opts = if state.shared do
      [shared_namespace_service: state.shared_namespace_service]
    else
      []
    end

    {:ok, conn_pid} = Nova.MCPConnection.start_link(client_socket, conn_opts)
    conn_id = make_ref()

    # Monitor the connection process
    Process.monitor(conn_pid)

    connections = Map.put(state.connections, conn_pid, %{
      id: conn_id,
      socket: client_socket,
      started_at: DateTime.utc_now()
    })

    IO.puts(:stderr, "Client connected (#{map_size(connections)} active connections)")

    {:noreply, %{state | connections: connections}}
  end

  def handle_info({:DOWN, _ref, :process, pid, reason}, state) do
    case Map.pop(state.connections, pid) do
      {nil, _} ->
        {:noreply, state}

      {_conn_info, connections} ->
        IO.puts(:stderr, "Client disconnected: #{inspect(reason)} (#{map_size(connections)} active connections)")
        {:noreply, %{state | connections: connections}}
    end
  end

  @impl true
  def terminate(_reason, state) do
    if state.listen_socket do
      :gen_tcp.close(state.listen_socket)
    end
    # In shared mode, stop the shared namespace service when server terminates
    if state.shared_namespace_service do
      GenServer.stop(state.shared_namespace_service, :normal)
    end
  end

  # Accept loop runs in a separate process
  defp accept_loop(parent, listen_socket) do
    case :gen_tcp.accept(listen_socket) do
      {:ok, client_socket} ->
        # Notify parent of new connection
        send(parent, {:new_connection, client_socket})
        # Continue accepting
        accept_loop(parent, listen_socket)

      {:error, :closed} ->
        # Server shutting down
        :ok

      {:error, reason} ->
        IO.puts(:stderr, "Accept error: #{inspect(reason)}")
        # Try again after a short delay
        Process.sleep(100)
        accept_loop(parent, listen_socket)
    end
  end
end

defmodule Nova.MCPConnection do
  @moduledoc """
  Handles a single MCP client connection.

  In isolated mode (default), each connection gets its own namespace service
  instance, providing isolation between different clients.

  In shared mode, all connections share the same namespace service, allowing
  real-time collaboration.
  """

  use GenServer
  require Logger

  defstruct [:socket, :namespace_service, :handler_state, :buffer, :owns_namespace_service]

  def start_link(socket, opts \\ []) do
    GenServer.start_link(__MODULE__, {socket, opts})
  end

  @impl true
  def init({socket, opts}) do
    # Use shared namespace service if provided, otherwise create our own
    {svc, owns_svc} = case Keyword.get(opts, :shared_namespace_service) do
      nil ->
        # Isolated mode: create our own namespace service
        {:ok, new_svc} = Nova.NamespaceService.start_link()
        {new_svc, true}
      shared_svc ->
        # Shared mode: use the shared namespace service
        {shared_svc, false}
    end

    # Initialize handler state (same as Nova.MCPServer.init)
    handler_state = %{
      namespace_service: svc,
      undo_stack: [],
      redo_stack: [],
      max_history: 50,
      sandboxes: %{}
    }

    state = %__MODULE__{
      socket: socket,
      namespace_service: svc,
      handler_state: handler_state,
      buffer: "",
      owns_namespace_service: owns_svc
    }

    # Start receiving messages
    send(self(), :receive)

    {:ok, state}
  end

  @impl true
  def handle_info(:receive, state) do
    case :gen_tcp.recv(state.socket, 0, 30_000) do
      {:ok, data} ->
        handle_data(data, state)

      {:error, :timeout} ->
        # Keep waiting
        send(self(), :receive)
        {:noreply, state}

      {:error, :closed} ->
        {:stop, :normal, state}

      {:error, reason} ->
        {:stop, reason, state}
    end
  end

  @impl true
  def terminate(_reason, state) do
    if state.socket do
      :gen_tcp.close(state.socket)
    end
    # Only stop the namespace service if we own it (isolated mode)
    # In shared mode, the server manages the shared namespace service
    if state.owns_namespace_service && state.namespace_service do
      GenServer.stop(state.namespace_service, :normal)
    end
    :ok
  end

  defp handle_data(data, state) do
    trimmed = String.trim(data)

    if trimmed == "" do
      send(self(), :receive)
      {:noreply, state}
    else
      IO.puts(:stderr, "[MCP RX] #{trimmed}")
      normalized = normalize_message(trimmed)

      case Jason.decode(normalized) do
        {:ok, request} ->
          # Extract request id for error responses
          request_id = Map.get(request, "id")

          # Wrap in try/catch to handle exceptions gracefully
          {response, new_handler_state} = try do
            process_request(request, state.handler_state)
          rescue
            e ->
              error_msg = Exception.message(e)
              IO.puts(:stderr, "[MCP ERROR] #{inspect(e.__struct__)}: #{error_msg}")
              IO.puts(:stderr, "[MCP ERROR] #{Exception.format_stacktrace(__STACKTRACE__)}")

              error_response = %{
                "jsonrpc" => "2.0",
                "id" => request_id,
                "error" => %{
                  "code" => -32603,
                  "message" => "Internal error: #{error_msg}"
                }
              }
              {error_response, state.handler_state}
          catch
            kind, reason ->
              IO.puts(:stderr, "[MCP ERROR] #{kind}: #{inspect(reason)}")

              error_response = %{
                "jsonrpc" => "2.0",
                "id" => request_id,
                "error" => %{
                  "code" => -32603,
                  "message" => "Internal error: #{inspect({kind, reason})}"
                }
              }
              {error_response, state.handler_state}
          end

          if response do
            json = Jason.encode!(response)
            IO.puts(:stderr, "[MCP TX] #{json}")
            :gen_tcp.send(state.socket, json <> "\n")
          end

          send(self(), :receive)
          {:noreply, %{state | handler_state: new_handler_state}}

        {:error, _} ->
          error_response = %{
            "jsonrpc" => "2.0",
            "error" => %{"code" => -32700, "message" => "Parse error"},
            "id" => nil
          }
          :gen_tcp.send(state.socket, Jason.encode!(error_response) <> "\n")
          send(self(), :receive)
          {:noreply, state}
      end
    end
  end

  # Normalize incoming JSON-RPC messages to ensure params field exists
  defp normalize_message(json) do
    case Jason.decode(json) do
      {:ok, %{"method" => _, "id" => _} = msg} when not is_map_key(msg, "params") ->
        msg |> Map.put("params", %{}) |> Jason.encode!()

      {:ok, %{"method" => _} = msg} when not is_map_key(msg, "params") ->
        msg |> Map.put("params", %{}) |> Jason.encode!()

      _ ->
        json
    end
  end

  # Process MCP requests using the Nova.MCPServer handler logic
  defp process_request(%{"method" => "initialize", "id" => id}, state) do
    result = %{
      protocolVersion: "2024-11-05",
      serverInfo: %{
        name: "nova-compiler",
        version: "0.1.0"
      },
      capabilities: %{
        tools: %{}
      }
    }

    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => result
    }

    {response, state}
  end

  defp process_request(%{"method" => "notifications/initialized"}, state) do
    {nil, state}
  end

  defp process_request(%{"method" => "tools/list", "id" => id}, state) do
    {:ok, tools, _new_state} = Nova.MCPServer.handle_list_tools(state)

    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{"tools" => tools}
    }

    {response, state}
  end

  defp process_request(%{"method" => "tools/call", "id" => id, "params" => params}, state) do
    tool_name = params["name"]
    arguments = params["arguments"] || %{}

    {:ok, result, new_state} = Nova.MCPServer.handle_call_tool(tool_name, arguments, state)

    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{"content" => result}
    }

    {response, new_state}
  end

  defp process_request(%{"method" => method, "id" => id}, state) do
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

  defp process_request(%{"method" => _method}, state) do
    # Notification without id - no response needed
    {nil, state}
  end
end
