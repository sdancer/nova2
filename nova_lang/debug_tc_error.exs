# Trace exactly where the TypeAliasInfo error occurs in TypeChecker

# Parse all needed modules
modules = [
  {"Ast", "../src/Nova/Compiler/Ast.purs"},
  {"Types", "../src/Nova/Compiler/Types.purs"},
  {"Unify", "../src/Nova/Compiler/Unify.purs"},
  {"TypeChecker", "../src/Nova/Compiler/TypeChecker.purs"},
]

parsed = Enum.reduce(modules, %{}, fn {_, path}, acc ->
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

# Get TypeChecker module
tc_mod = Map.get(parsed, "Nova.Compiler.TypeChecker")

# Find functions that use TypeAliasInfo in their signature
funcs_with_tai = Enum.filter(tc_mod.declarations, fn
  {:decl_function, func} ->
    case func.type_signature do
      {:just, sig} ->
        # Check if TypeAliasInfo appears in the type
        type_str = inspect(sig.ty)
        String.contains?(type_str, "TypeAliasInfo")
      _ -> false
    end
  {:decl_type_sig, sig} ->
    type_str = inspect(sig.ty)
    String.contains?(type_str, "TypeAliasInfo")
  _ -> false
end)

IO.puts("Functions/signatures mentioning TypeAliasInfo:")
for decl <- funcs_with_tai do
  case decl do
    {:decl_function, func} -> IO.puts("  Function: #{func.name}")
    {:decl_type_sig, sig} -> IO.puts("  TypeSig: #{sig.name}")
    _ -> :ok
  end
end
