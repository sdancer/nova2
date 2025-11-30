defmodule Nova.MCPTcpTransport do
  @moduledoc """
  Server-side TCP transport for MCP.

  Listens on a TCP port and accepts a single client connection.
  Uses newline-delimited JSON protocol like stdio transport.

  This avoids Elixir stdout pollution breaking the MCP protocol.
  """

  @behaviour ExMCP.Transport

  require Logger

  defstruct [:listen_socket, :client_socket, :buffer, :port]

  @default_port 9999

  @impl true
  def connect(opts) do
    port = Keyword.get(opts, :port, @default_port)

    # Print port to stderr so it doesn't interfere with any remaining stdout usage
    IO.puts(:stderr, "Nova MCP server listening on port #{port}")

    case :gen_tcp.listen(port, [
      :binary,
      packet: :line,
      active: false,
      reuseaddr: true
    ]) do
      {:ok, listen_socket} ->
        IO.puts(:stderr, "Waiting for client connection...")

        case :gen_tcp.accept(listen_socket) do
          {:ok, client_socket} ->
            IO.puts(:stderr, "Client connected")
            {:ok, %__MODULE__{
              listen_socket: listen_socket,
              client_socket: client_socket,
              buffer: "",
              port: port
            }}

          {:error, reason} ->
            :gen_tcp.close(listen_socket)
            {:error, {:accept_failed, reason}}
        end

      {:error, reason} ->
        {:error, {:listen_failed, reason}}
    end
  end

  @impl true
  def send_message(message, state) do
    # MCP uses newline-delimited JSON
    IO.puts(:stderr, "[MCP TX] #{message}")
    case :gen_tcp.send(state.client_socket, message <> "\n") do
      :ok ->
        {:ok, state}
      {:error, reason} ->
        {:error, reason}
    end
  end

  @impl true
  def receive_message(state) do
    case :gen_tcp.recv(state.client_socket, 0) do
      {:ok, data} ->
        trimmed = String.trim(data)
        if trimmed == "" do
          # Empty line, keep reading
          receive_message(state)
        else
          IO.puts(:stderr, "[MCP RX] #{trimmed}")
          # Normalize message: ensure params field exists for requests
          # ExMCP's Protocol.parse_message requires params field
          normalized = normalize_message(trimmed)
          {:ok, normalized, state}
        end

      {:error, :closed} ->
        {:error, :eof}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Normalize incoming JSON-RPC messages to ensure params field exists
  # This works around a limitation in ExMCP.Protocol.parse_message
  defp normalize_message(json) do
    case Jason.decode(json) do
      {:ok, %{"method" => _, "id" => _} = msg} when not is_map_key(msg, "params") ->
        # Request without params - add empty params
        msg
        |> Map.put("params", %{})
        |> Jason.encode!()

      {:ok, %{"method" => _} = msg} when not is_map_key(msg, "params") ->
        # Notification without params - add empty params
        msg
        |> Map.put("params", %{})
        |> Jason.encode!()

      _ ->
        # Already has params or not a request/notification - pass through
        json
    end
  end

  @impl true
  def close(state) do
    if state.client_socket do
      :gen_tcp.close(state.client_socket)
    end
    if state.listen_socket do
      :gen_tcp.close(state.listen_socket)
    end
    :ok
  end

  @impl true
  def connected?(state) do
    state.client_socket != nil
  end
end
