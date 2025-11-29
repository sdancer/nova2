# Debug import resolution

# Parse Types.purs
types_src = File.read!("../src/Nova/Compiler/Types.purs")
types_tokens = Nova.Compiler.Tokenizer.tokenize(types_src)
{:right, {:tuple, types_mod, _}} = Nova.Compiler.Parser.parse_module(types_tokens)
IO.puts("Types.purs: #{length(types_mod.declarations)} declarations")

# Extract exports
exports = Nova.Compiler.TypeChecker.extract_exports(types_mod.declarations)
IO.puts("\nTypes exports:")
IO.puts("  Types: #{inspect(Map.keys(exports.types))}")
IO.puts("  Constructors: #{inspect(Map.keys(exports.constructors))}")
IO.puts("  Type aliases: #{inspect(Map.keys(exports.type_aliases))}")

# Check if TVar is in type_aliases
IO.puts("\nTVar alias: #{inspect(Map.get(exports.type_aliases, "TVar"))}")

# Parse Unify.purs
unify_src = File.read!("../src/Nova/Compiler/Unify.purs")
unify_tokens = Nova.Compiler.Tokenizer.tokenize(unify_src)
{:right, {:tuple, unify_mod, _}} = Nova.Compiler.Parser.parse_module(unify_tokens)

# Find the import declaration for Types
imports = Enum.filter(unify_mod.declarations, fn
  {:decl_import, _} -> true
  _ -> false
end)
IO.puts("\nUnify imports:")
for {:decl_import, imp} <- imports do
  IO.puts("  #{imp.module_name}: #{inspect(imp.items)}")
end
