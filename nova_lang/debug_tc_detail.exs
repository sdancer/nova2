# Debug type checking TypeChecker step by step

# Parse all needed modules
modules = [
  {"Ast", "../src/Nova/Compiler/Ast.purs"},
  {"Types", "../src/Nova/Compiler/Types.purs"},
  {"Unify", "../src/Nova/Compiler/Unify.purs"},
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

# Build full registry
registry = Enum.reduce(parsed, %{}, fn {mod_name, mod}, reg ->
  exports = Nova.Compiler.TypeChecker.extract_exports(mod.declarations)
  Nova.Compiler.Types.register_module(reg, mod_name, exports)
end)

# Get TypeChecker declarations
tc_mod = Map.get(parsed, "Nova.Compiler.TypeChecker")

# Collect imported aliases
imported_aliases = Nova.Compiler.TypeChecker.collect_imported_aliases(registry, tc_mod.declarations)
IO.puts("Imported TypeAliasInfo body:")
tai = Map.get(imported_aliases, "TypeAliasInfo")
IO.inspect(tai)

# Try converting it to Type manually
if tai do
  IO.puts("\nConverting TypeAliasInfo to Type:")
  # type_expr_to_type expects varMap as first arg
  converted = Nova.Compiler.TypeChecker.type_expr_to_type(%{}, tai.body)
  IO.inspect(converted)
end
