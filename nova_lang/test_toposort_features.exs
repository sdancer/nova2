# Test features from topoSortBinds

# 1. Comment in let binding
test1 = """
module Test where
foo x =
  let -- comment
      a = 1
  in a
"""

# 2. Multiple let bindings with comments
test2 = """
module Test where
foo x =
  let -- Build map
      bindMap = f x
      -- Get names
      names = g x
  in names
"""

# 3. Nested let
test3 = """
module Test where
foo binds =
  let deps = map (\\b ->
        let name = getName b
        in name) binds
  in deps
"""

# 4. Record punning { name, ... }
test4 = """
module Test where
foo name =
  let x = 1
  in { name, x: x }
"""

# 5. Full pattern from source
test5 = """
module Test where

topoSortBinds binds =
  let bindMap = f binds
      bindNames = g bindMap
      deps = map (\\b ->
        let name = fromMaybe "" (getPatternVarName b.pattern)
            freeVars = freeVarsInExprFor bindNames Set.empty b.value
            localDeps = freeVars
        in { name, bind: b, deps: localDeps }) binds
      sorted = kahnSort deps
  in sorted
"""

for {label, code} <- [
  {"comment in let", test1},
  {"multi bindings with comments", test2},
  {"nested let in lambda", test3},
  {"record punning", test4},
  {"full topoSortBinds", test5}
] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 60)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("#{label}: #{result}")
end
