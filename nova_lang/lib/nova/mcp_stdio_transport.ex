defmodule Nova.MCPStdioTransport do
  @moduledoc """
  Server-side stdio transport for MCP.

  Unlike the client-side ExMCP.Transport.Stdio which spawns a subprocess,
  this transport reads from stdin and writes to stdout directly, making it
  suitable for running as an MCP server.
  """

  @behaviour ExMCP.Transport

  defstruct [:buffer]

  @impl true
  def connect(_opts) do
    # Configure stdin for binary mode
    :io.setopts(:standard_io, [:binary])
    {:ok, %__MODULE__{buffer: ""}}
  end

  @impl true
  def send_message(message, state) do
    # MCP uses newline-delimited JSON
    IO.puts(message)
    {:ok, state}
  end

  @impl true
  def receive_message(state) do
    case IO.gets("") do
      :eof ->
        {:error, :eof}

      {:error, reason} ->
        {:error, reason}

      line when is_binary(line) ->
        trimmed = String.trim(line)
        if trimmed == "" do
          # Empty line, keep reading
          receive_message(state)
        else
          {:ok, trimmed, state}
        end
    end
  end

  @impl true
  def close(_state) do
    :ok
  end
end
