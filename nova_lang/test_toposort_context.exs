source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Extract lines 135-166 (the where clause area plus topoSortBinds start)
context_lines = Enum.slice(lines, 135..166)
test_code = "module Test where\n\n" <> Enum.join(context_lines, "\n")

tokens = Nova.Compiler.Tokenizer.tokenize(test_code)
IO.puts("Context test tokens: #{length(tokens)}")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} ->
    IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
    for d <- mod.declarations do
      case d do
        {:decl_function, f} -> IO.puts("  Function: #{f.name}")
        {:decl_type_sig, s} -> IO.puts("  TypeSig: #{s.name}")
        _ -> :ok
      end
    end
end

# Show what lines 135-166 are
IO.puts("\nLines 136-166 preview:")
for {line, i} <- Enum.with_index(Enum.slice(lines, 135..165), 136) do
  IO.puts("  #{i}: #{String.slice(line, 0, 60)}")
end
