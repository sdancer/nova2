source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

code_150 = Enum.take(lines, 150) |> Enum.join("\n")
tokens_150 = Nova.Compiler.Tokenizer.tokenize(code_150)
IO.puts("Lines 1-150 total tokens: #{length(tokens_150)}")

# Try parsing with first 380 tokens
tokens_380 = Enum.take(tokens_150, 380)
IO.puts("\nParsing first 380 tokens from lines 1-150:")

case Nova.Compiler.Parser.parse_module(tokens_380) do
  {:left, err} -> IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} ->
    IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end

# Try a few token counts around 366
for n <- [365, 366, 367, 368, 370, 375, 380] do
  toks = Enum.take(tokens_150, n)
  result = case Nova.Compiler.Parser.parse_module(toks) do
    {:left, _} -> "FAIL"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("#{n} tokens: #{result}")
end
