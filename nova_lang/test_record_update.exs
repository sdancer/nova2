# Test case clause with ExprRecordUpdate pattern

test1 = """
module Test where

foo expr = case expr of
  ExprList exprs -> 1
  ExprTuple exprs -> 2
  ExprRecord fields -> 3
  ExprRecordAccess e _ -> 4
  ExprRecordUpdate e updates -> 5
  ExprTyped e _ -> 6
"""

test2 = """
module Test where

foo expr = case expr of
  ExprRecordAccess e _ -> 4
  ExprRecordUpdate e updates -> 5
"""

test3 = """
module Test where

foo expr = case expr of
  ExprRecordUpdate e updates -> 5
"""

test4 = """
module Test where

foo expr = case expr of
  ExprRecord fields -> foldr (\\(Tuple _ e) s -> f e s) Set.empty fields
  ExprRecordAccess e _ -> f e
  ExprRecordUpdate e updates -> Set.union (f e) (foldr (\\(Tuple _ v) s -> f v s) Set.empty updates)
"""

for {label, code} <- [{"6 clauses", test1}, {"2 clauses", test2}, {"1 clause", test3}, {"complex", test4}] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 60)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("#{label}: #{result}")
end
