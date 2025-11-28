# Simplify to find the issue

# Full version (fails)
test_full = """
module Test where
topoSortBinds binds =
  let bindMap = f binds
      deps = map (\\b ->
        let name = getName b
            freeVars = getVars b
        in { name, deps: freeVars }) binds
      sorted = kahnSort deps
  in sorted
  where
    kahnSort items = items
"""

# Without nested let
test_no_nested_let = """
module Test where
topoSortBinds binds =
  let bindMap = f binds
      sorted = kahnSort bindMap
  in sorted
  where
    kahnSort items = items
"""

# Without record punning
test_no_punning = """
module Test where
topoSortBinds binds =
  let deps = map (\\b ->
        let name = getName b
        in { name: name }) binds
      sorted = kahnSort deps
  in sorted
  where
    kahnSort items = items
"""

# Without where clause
test_no_where = """
module Test where
topoSortBinds binds =
  let bindMap = f binds
      deps = map (\\b ->
        let name = getName b
        in { name, deps: name }) binds
      sorted = kahnSort deps
  in sorted
"""

# Just let-in with where
test_simple_let_where = """
module Test where
foo x =
  let y = f x
  in y
  where
    f a = a + 1
"""

for {label, code} <- [
  {"full version", test_full},
  {"no nested let", test_no_nested_let},
  {"no punning", test_no_punning},
  {"no where", test_no_where},
  {"simple let-where", test_simple_let_where}
] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("#{label}: #{result}")
end
