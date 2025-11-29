# Debug type alias resolution

# Parse all modules
modules = [
  {"Ast", "../src/Nova/Compiler/Ast.purs"},
  {"Types", "../src/Nova/Compiler/Types.purs"},
  {"Unify", "../src/Nova/Compiler/Unify.purs"},
]

parsed = Enum.reduce(modules, %{}, fn {name, path}, acc ->
  src = File.read!(path)
  tokens = Nova.Compiler.Tokenizer.tokenize(src)
  case Nova.Compiler.Parser.parse_module(tokens) do
    {:right, {:tuple, mod, _}} ->
      Map.put(acc, mod.name, mod)
    {:left, err} ->
      IO.puts("Failed to parse #{name}: #{inspect(err)}")
      acc
  end
end)

# Build registry
registry = Enum.reduce(parsed, %{}, fn {mod_name, mod}, reg ->
  exports = Nova.Compiler.TypeChecker.extract_exports(mod.declarations)
  Nova.Compiler.Types.register_module(reg, mod_name, exports)
end)

# Check what imported aliases are collected for Unify
unify_mod = Map.get(parsed, "Nova.Compiler.Unify")
imported_aliases = Nova.Compiler.TypeChecker.collect_imported_aliases(registry, unify_mod.declarations)
IO.puts("\n=== Imported aliases for Unify ===")
for {name, info} <- imported_aliases do
  IO.puts("  #{name}: params=#{inspect(info.params)}, body=#{inspect(info.body)}")
end

# Check the Types module exports specifically
types_exports = Map.get(registry, "Nova.Compiler.Types")
IO.puts("\n=== Types module type_aliases ===")
for {name, info} <- types_exports.type_aliases do
  IO.puts("  #{name}: params=#{inspect(info.params)}, body=#{inspect(info.body)}")
end
