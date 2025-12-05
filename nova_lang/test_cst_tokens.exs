# Test to see what tokens are generated for a case expression
code = """
module Test where

foo x =
  case x of
    Nothing -> 1
    Just y -> y

bar = 1
"""

tokens = Nova.Compiler.CstLexer.lex_module(code)

IO.puts("Tokens generated:")
tokens
|> Enum.with_index()
|> Enum.each(fn {tok, i} ->
  IO.puts("  #{i}: #{inspect(tok.value)}")
end)
