# Test comments inside let bindings

test1 = """
module Test where
foo x =
  let a = 1
  in a
"""

test2 = """
module Test where
foo x =
  let -- comment before first binding
      a = 1
  in a
"""

test3 = """
module Test where
foo x =
  let a = 1
      -- comment between bindings
      b = 2
  in b
"""

test4 = """
module Test where
foo x =
  let -- first comment
      a = 1
      -- second comment
      b = 2
  in b
"""

test5 = """
module Test where
foo binds =
  let -- Build map
      bindMap = f binds
      -- Get names
      bindNames = g bindMap
  in bindNames
"""

for {label, code} <- [
  {"no comments", test1},
  {"comment before first binding", test2},
  {"comment between bindings", test3},
  {"multiple comments", test4},
  {"like topoSortBinds", test5}
] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 50)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("#{label}: #{result}")
end
