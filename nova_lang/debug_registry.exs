# Debug registry-based type checking

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
      IO.puts("Parsed #{name}: #{length(mod.declarations)} declarations")
      Map.put(acc, mod.name, mod)
    {:left, err} ->
      IO.puts("Failed to parse #{name}: #{inspect(err)}")
      acc
  end
end)

IO.puts("\n=== Building Registry ===")
registry = Enum.reduce(parsed, %{}, fn {mod_name, mod}, reg ->
  exports = Nova.Compiler.TypeChecker.extract_exports(mod.declarations)
  IO.puts("#{mod_name}: #{map_size(exports.constructors)} ctors, #{map_size(exports.types)} types, #{map_size(exports.type_aliases)} aliases")
  Nova.Compiler.Types.register_module(reg, mod_name, exports)
end)

IO.puts("\n=== Type Checking Unify ===")
unify_mod = Map.get(parsed, "Nova.Compiler.Unify")
env = Nova.Compiler.Types.empty_env()
result = Nova.Compiler.TypeChecker.check_module_with_registry(registry, env, unify_mod.declarations)
case result do
  {:right, _} -> IO.puts("Unify: OK")
  {:left, err} -> IO.puts("Unify: FAILED - #{inspect(err)}")
end
