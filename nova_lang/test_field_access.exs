# Test field access on function results

test1 = """
module Test where
foo x = (bar x).str
"""

test2 = """
module Test where
foo :: Int -> String
foo x = (bar x).str
"""

test3 = """
module Test where
genPattern :: Pattern -> String
genPattern pat = (genPatternWithCounter pat 0).str
"""

test4 = """
module Test where
foo x = (bar x 0).str
bar y z = { str: "hello", counter: z }
"""

for {label, code} <- [
  {"simple field access", test1},
  {"with type sig", test2},
  {"genPattern pattern", test3},
  {"two functions", test4}
] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 50)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("#{label}: #{result}")
end
