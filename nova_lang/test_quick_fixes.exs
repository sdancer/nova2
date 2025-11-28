# Test fixes

test_compose = """
module Test where
foo = not <<< bar
"""

test_field_access = """
module Test where
foo x = (bar x).str
"""

for {label, code} <- [
  {"compose <<<", test_compose},
  {"field access", test_field_access}
] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 50)}"
    {:right, {:tuple, mod, _}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("#{label}: #{result}")
end
