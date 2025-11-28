# Test direct compilation of compiler files
base_path = "/home/sdancer/nova2/src/Nova/Compiler"
compiler_files = [
  {"Ast", []},
  {"Types", ["Ast"]},
  {"Tokenizer", []},
  {"Unify", ["Types"]},
  {"TypeChecker", ["Ast", "Types", "Unify"]},
  {"CodeGen", ["Ast"]},
  {"Parser", ["Ast", "Tokenizer"]},
  {"Dependencies", ["Ast"]},
]

IO.puts("=== Direct Compilation Test ===\n")

Enum.each(compiler_files, fn {name, deps} ->
  path = Path.join(base_path, "#{name}.purs")
  IO.puts("Compiling #{name}...")

  # Read dependencies
  dep_sources = Enum.map(deps, fn dep ->
    dep_path = Path.join(base_path, "#{dep}.purs")
    case File.read(dep_path) do
      {:ok, src} -> src
      _ -> ""
    end
  end)

  # Read source
  case File.read(path) do
    {:ok, source} ->
      result = Nova.compile(source, dep_sources)
      case result do
        {:ok, elixir_code} ->
          lines = elixir_code |> String.split("\n") |> length()
          IO.puts("  OK: Generated #{lines} lines")

        {:error, {:parse, err}} ->
          IO.puts("  PARSE ERROR: #{inspect(err) |> String.slice(0, 100)}")

        {:error, {:typecheck, err}} ->
          IO.puts("  TYPE ERROR: #{inspect(err) |> String.slice(0, 100)}")

        {:error, err} ->
          IO.puts("  ERROR: #{inspect(err) |> String.slice(0, 100)}")
      end

    {:error, reason} ->
      IO.puts("  FILE ERROR: #{inspect(reason)}")
  end
end)

IO.puts("\n=== Done ===")
