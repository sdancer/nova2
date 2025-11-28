test_code = """
module Test where

foo expr = case expr of
  1 -> "one"
  2 -> "two"
  where
    helper = "test"
"""

tokens = Nova.Compiler.Tokenizer.tokenize(test_code)
IO.puts("Tokens: #{length(tokens)}")

# Show tokens around "where"
tokens
|> Enum.with_index()
|> Enum.filter(fn {tok, _} -> tok.value == "where" end)
|> Enum.each(fn {tok, idx} -> IO.puts("where at idx #{idx}: col=#{tok.column}, line=#{tok.line}") end)

case Nova.Compiler.Parser.parse_module(tokens) do
  {:_left, err} -> IO.puts("Parse error: #{inspect(err)}")
  {:left, err} -> IO.puts("Parse error: #{inspect(err)}")
  {:_right, {:_tuple, mod, _}} -> IO.puts("Parsed module: #{mod.name}")
  {:right, {:_tuple, mod, _}} -> IO.puts("Parsed module: #{mod.name}")
  other -> IO.puts("Unexpected: #{inspect(other)}")
end
