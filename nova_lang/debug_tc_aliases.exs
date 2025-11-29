# Debug type alias resolution for TypeChecker

# Parse modules
modules = [
  {"Types", "../src/Nova/Compiler/Types.purs"},
  {"TypeChecker", "../src/Nova/Compiler/TypeChecker.purs"},
]

parsed = Enum.reduce(modules, %{}, fn {name, path}, acc ->
  src = File.read!(path)
  tokens = Nova.Compiler.Tokenizer.tokenize(src)
  case Nova.Compiler.Parser.parse_module(tokens) do
    {:right, {:tuple, mod, _}} ->
      Map.put(acc, mod.name, mod)
    _ -> acc
  end
end)

# Build registry with Types
types_mod = Map.get(parsed, "Nova.Compiler.Types")
types_exports = Nova.Compiler.TypeChecker.extract_exports(types_mod.declarations)
registry = Nova.Compiler.Types.register_module(%{}, "Nova.Compiler.Types", types_exports)

IO.puts("=== Types module type_aliases ===")
for {name, _} <- types_exports.type_aliases do
  IO.puts("  #{name}")
end

# Check what imported aliases TypeChecker gets
tc_mod = Map.get(parsed, "Nova.Compiler.TypeChecker")
imported_aliases = Nova.Compiler.TypeChecker.collect_imported_aliases(registry, tc_mod.declarations)
IO.puts("\n=== Imported aliases for TypeChecker ===")
for {name, _} <- imported_aliases do
  IO.puts("  #{name}")
end
