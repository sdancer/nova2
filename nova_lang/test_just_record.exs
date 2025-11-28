# Test Just with record literal

test1 = """
module Test where
foo = Just { mod: "functor", func: "map", arity: 2 }
"""

test2 = """
module Test where
getPreludeFunc "map" = Just { mod: "functor", func: "map", arity: 2 }
getPreludeFunc "filter" = Just { mod: "lists", func: "filter", arity: 2 }
"""

test3 = """
module Test where
getPreludeFunc "foldl" = Just { mod: "functor", func: "foldl", arity: 3 }
getPreludeFunc "foldr" = Just { mod: "functor", func: "foldr", arity: 3 }
getPreludeFunc "map" = Just { mod: "functor", func: "map", arity: 2 }
getPreludeFunc "filter" = Just { mod: "lists", func: "filter", arity: 2 }
"""

for {label, code} <- [{"single", test1}, {"two clauses", test2}, {"four clauses", test3}] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 60)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("#{label}: #{result}")
end
