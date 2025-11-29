# Time each compilation step for each compiler file
base_path = "/home/sdancer/nova2/src/Nova/Compiler"

files = [
  {"Ast", []},
  {"Types", ["Ast"]},
  {"Tokenizer", []},
  {"Unify", ["Types"]},
  {"TypeChecker", ["Ast", "Types", "Unify"]},
  {"CodeGen", ["Ast"]},
  {"Parser", ["Ast", "Tokenizer"]},
  {"Dependencies", ["Ast"]},
]

defmodule Timer do
  def time(label, fun) do
    {microseconds, result} = :timer.tc(fun)
    ms = microseconds / 1000
    IO.puts("  #{label}: #{Float.round(ms, 1)}ms")
    result
  end
end

IO.puts("=== Compilation Timing Test ===\n")

Enum.each(files, fn {name, deps} ->
  path = Path.join(base_path, "#{name}.purs")
  IO.puts("#{name}:")

  # Read file
  source = Timer.time("read file", fn -> File.read!(path) end)
  IO.puts("    source: #{String.length(source)} chars, #{length(String.split(source, "\n"))} lines")

  # Read deps
  dep_sources = Timer.time("read deps", fn ->
    Enum.map(deps, fn dep ->
      dep_path = Path.join(base_path, "#{dep}.purs")
      File.read!(dep_path)
    end)
  end)

  # Tokenize
  tokens = Timer.time("tokenize", fn ->
    Nova.Compiler.Tokenizer.tokenize(source)
  end)
  IO.puts("    tokens: #{length(tokens)}")

  # Parse
  parse_result = Timer.time("parse", fn ->
    Nova.Compiler.Parser.parse_module(tokens)
  end)

  case parse_result do
    {:right, {:tuple, mod, _}} ->
      IO.puts("    declarations: #{length(mod.declarations)}")

      # Full compile (typecheck + codegen via Nova.compile)
      Timer.time("typecheck+codegen", fn ->
        Nova.compile(source, dep_sources)
      end)
      IO.puts("")

    {:left, err} ->
      IO.puts("  PARSE ERROR: #{inspect(err) |> String.slice(0, 80)}\n")
  end
end)

IO.puts("=== Done ===")
