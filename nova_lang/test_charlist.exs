# Test char literals in lists

test1 = """
module Test where
foo = ['a']
"""

test2 = """
module Test where
foo = ['_']
"""

test3 = """
module Test where
foo c = ['_', c]
"""

test4 = """
module Test where
foo c = if c > 'A'
        then ['_', toLower c]
        else [c]
"""

for {label, code} <- [{"simple char", test1}, {"underscore char", test2}, {"list pair", test3}, {"if-then char", test4}] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 60)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("#{label}: #{result}")
end
