# Test MCP Server implementation
IO.puts("=== Nova MCP Server Test ===\n")

core_dir = "priv/core"

load_core_module = fn path ->
  case File.read(path) do
    {:ok, content} ->
      case Nova.CoreErlangEval.compile_and_load(content) do
        {:ok, mod_name} -> {:ok, mod_name}
        {:error, reason} -> {:error, path, reason}
      end
    {:error, reason} -> {:error, path, reason}
  end
end

# Load required modules
modules = [
  "Data/Maybe.core",
  "Data/Either.core",
  "Data/String.core",
  "Data/Array.core",
  "Data/Map.core",
  "MCP/Json.core",
  "MCP/JsonRpc.core",
  "MCP/Transport.core",
  "MCP/Tools.core",
  "MCP/Server.core"
]

IO.puts("Loading modules...")
results = Enum.map(modules, fn rel_path ->
  path = Path.join(core_dir, rel_path)
  IO.write("  #{rel_path}... ")
  case load_core_module.(path) do
    {:ok, mod} ->
      IO.puts("OK (#{mod})")
      {:ok, mod}
    {:error, p, r} ->
      IO.puts("FAILED: #{inspect(r)}")
      {:error, p, r}
  end
end)

loaded = Enum.count(results, &match?({:ok, _}, &1))
failed = Enum.count(results, &match?({:error, _, _}, &1))

IO.puts("\n=== Summary ===")
IO.puts("Loaded: #{loaded}/#{length(modules)}")

if failed > 0 do
  IO.puts("Failed: #{failed}")
  IO.puts("\n=== MCP Test FAILED ===")
else
  IO.puts("\n=== Testing MCP JSON ===")

  # Test JSON encode
  test_map = %{"name" => "test", "version" => 1}
  encoded = apply(:"MCP.Json", :encode, [test_map])
  IO.puts("Encoded: #{encoded}")

  # Test JSON decode (Nova uses charlists, not binaries)
  decoded = apply(:"MCP.Json", :decode, [~c"{\"hello\":\"world\"}"])
  IO.puts("Decoded: #{inspect(decoded)}")

  IO.puts("\n=== MCP Test PASSED ===")
end
