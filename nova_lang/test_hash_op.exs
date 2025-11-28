# Test the # operator (reverse application)

test1 = """
module Test where
foo x = x # f
"""

test2 = """
module Test where
foo = getPatternVarName b.pattern # map (\\n -> Tuple n b)
"""

test3 = """
module Test where
foo binds =
  let bindMap = Map.fromFoldable (Array.mapMaybe (\\b -> getPatternVarName b.pattern # map (\\n -> Tuple n b)) binds)
  in bindMap
"""

for {label, code} <- [{"simple #", test1}, {"nested #", test2}, {"in let", test3}] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 60)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("#{label}: #{result}")
end
