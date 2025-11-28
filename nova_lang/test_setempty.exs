# Test Set.empty at end of case clause body

test1 = """
module Test where

foo expr = case expr of
  ExprLit _ -> Set.empty
  ExprApp f arg -> f arg
"""

test2 = """
module Test where

foo expr = case expr of
  ExprVar name ->
    if name == "_"
    then Set.singleton name
    else Set.empty
  ExprLit _ -> Set.empty
  ExprApp f arg -> f arg
"""

test3 = """
module Test where

import Data.Set as Set

foo candidates bound expr = case expr of
  ExprVar name ->
    if Set.member name candidates && not (Set.member name bound)
    then Set.singleton name
    else Set.empty
  ExprLit _ -> Set.empty
  ExprApp f arg -> Set.union (foo candidates bound f) (foo candidates bound arg)
"""

for {label, code} <- [{"simple", test1}, {"with if", test2}, {"full", test3}] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 60)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("#{label}: #{result}")
end
