modules = [
  "../src/Nova/Compiler/Ast.purs",
  "../src/Nova/Compiler/Types.purs",
  "../src/Nova/Compiler/Tokenizer.purs",
  "../src/Nova/Compiler/Unify.purs",
  "../src/Nova/Compiler/TypeChecker.purs",
  "../src/Nova/Compiler/CodeGen.purs",
  "../src/Nova/Compiler/CodeGenCoreErlang.purs",
  "../src/Nova/Compiler/Parser.purs",
  "../src/Nova/Compiler/Dependencies.purs"
]

total_decls = 0

for path <- modules do
  source = File.read!(path)
  tokens = Nova.Compiler.Tokenizer.tokenize(source)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> {:error, inspect(err)}
    {:right, {:tuple, mod, rest}} ->
      if length(rest) > 0 do
        {:error, "#{length(rest)} remaining tokens"}
      else
        {:ok, length(mod.declarations)}
      end
  end

  name = Path.basename(path)
  case result do
    {:ok, n} ->
      total_decls = total_decls + n
      IO.puts("✓ #{name}: #{n} declarations")
    {:error, msg} ->
      IO.puts("✗ #{name}: #{msg}")
  end
end

IO.puts("\nTotal: #{length(modules)} modules")
