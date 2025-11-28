test_code = """
module Test where

foo (PatVar name) n | name == "_" = 1
foo (PatVar name) n = 2
"""

tokens = Nova.Compiler.Tokenizer.tokenize(test_code)
IO.puts("Tokens: #{length(tokens)}")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} -> IO.puts("Parse error: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} ->
    IO.puts("Parsed module: #{mod.name}")
    IO.puts("Declarations: #{length(mod.declarations)}")
end
