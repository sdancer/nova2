# Test let-in followed by function-level where

test1 = """
module Test where
foo x =
  let a = 1
  in a
  where
    helper = 2
"""

test2 = """
module Test where
foo x =
  let a = f x
      b = g x
      c = h a b
  in c
  where
    f y = y + 1
    g y = y * 2
    h p q = p + q
"""

test3 = """
module Test where
topoSortBinds binds =
  let sorted = kahnSort deps
  in sorted
  where
    kahnSort items = items
"""

for {label, code} <- [{"simple let-where", test1}, {"multi-binding let-where", test2}, {"topoSort pattern", test3}] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 60)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("#{label}: #{result}")
end
