# Test guards in pattern matching

test1 = """
module Test where
foo x | x > 0 = 1
foo x = 0
"""

test2 = """
module Test where
foo (Just x) | x > 0 = 1
foo _ = 0
"""

test3 = """
module Test where
genPatternWithCounter (PatVar name) n | take 1 name == "_" = { str: "_W", counter: n + 1 }
genPatternWithCounter (PatVar name) n = { str: name, counter: n }
"""

test4 = """
module Test where
foo :: Int -> Int
foo x | x > 0 = 1
foo x = 0
"""

for {label, code} <- [
  {"simple guard", test1},
  {"pattern + guard", test2},
  {"multi-clause with guard", test3},
  {"guard with type sig", test4}
] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 50)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("#{label}: #{result}")
end
