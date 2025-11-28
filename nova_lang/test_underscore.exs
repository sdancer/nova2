# Test underscore accessor syntax

test1 = """
module Test where
foo xs = map _.name xs
"""

test2 = """
module Test where
foo xs = map (\\x -> x.name) xs
"""

test3 = """
module Test where
foo = not <<< bar
"""

test4 = """
module Test where
foo x = bar x # baz
"""

for {label, code} <- [
  {"underscore accessor", test1},
  {"explicit lambda", test2},
  {"compose", test3},
  {"pipe operator", test4}
] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 50)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("#{label}: #{result}")
end
