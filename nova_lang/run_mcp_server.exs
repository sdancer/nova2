# Nova MCP Server Launcher
# Usage: mix run run_mcp_server.exs

defmodule MCPLauncher do
  @core_dir "priv/core"

  @modules [
    # Dependencies
    "Data/Maybe.core",
    "Data/Either.core",
    "Data/String.core",
    "Data/Array.core",
    "Data/Map.core",
    # MCP modules
    "MCP/Json.core",
    "MCP/Server.core"
  ]

  def run do
    IO.puts(:stderr, "Loading Nova MCP modules...")

    case load_all_modules() do
      {:ok, count} ->
        IO.puts(:stderr, "Loaded #{count} modules")
        IO.puts(:stderr, "Starting Nova MCP Server (STDIO)...")
        apply(:"MCP.Server", :runStdio, [:unit])

      {:error, failed} ->
        IO.puts(:stderr, "Failed to load modules: #{inspect(failed)}")
        System.halt(1)
    end
  end

  defp load_all_modules do
    results = Enum.map(@modules, fn rel_path ->
      path = Path.join(@core_dir, rel_path)
      case File.read(path) do
        {:ok, content} ->
          case Nova.CoreErlangEval.compile_and_load(content) do
            {:ok, mod} -> {:ok, mod}
            {:error, reason} -> {:error, {rel_path, reason}}
          end
        {:error, reason} -> {:error, {rel_path, reason}}
      end
    end)

    failed = Enum.filter(results, &match?({:error, _}, &1))

    if Enum.empty?(failed) do
      {:ok, length(results)}
    else
      {:error, failed}
    end
  end
end

MCPLauncher.run()
