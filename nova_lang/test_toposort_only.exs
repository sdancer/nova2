source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Extract lines 147-170 (just topoSortBinds)
context_lines = Enum.slice(lines, 146..169)
test_code = "module Test where\n\n" <> Enum.join(context_lines, "\n")

IO.puts("Test code:")
IO.puts(test_code)
IO.puts("\n--- Parsing ---")

tokens = Nova.Compiler.Tokenizer.tokenize(test_code)
IO.puts("Tokens: #{length(tokens)}")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} ->
    IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end
