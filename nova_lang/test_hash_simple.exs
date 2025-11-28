test = """
module Test where
foo x = x # f
"""

tokens = Nova.Compiler.Tokenizer.tokenize(test)
IO.puts("Tokens: #{length(tokens)}")
for t <- tokens do
  IO.puts("  #{t.token_type}: '#{t.value}'")
end

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} -> IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} -> IO.puts("OK: #{length(mod.declarations)} decls")
end
