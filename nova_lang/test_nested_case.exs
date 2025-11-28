# Test the exact pattern from CodeGenCoreErlang.purs lines 93-135

test_full = """
module Test where

import Data.Set as Set

freeVarsInExprFor candidates bound expr = case expr of
  ExprVar name ->
    if Set.member name candidates && not (Set.member name bound)
    then Set.singleton name
    else Set.empty
  ExprLit _ -> Set.empty
  ExprApp f arg -> Set.union (freeVarsInExprFor candidates bound f) (freeVarsInExprFor candidates bound arg)
  ExprLambda pats body ->
    let patBound = foldr addPatternVars bound pats
    in freeVarsInExprFor candidates patBound body
  ExprLet binds body ->
    let bindNames = Set.fromFoldable (Array.mapMaybe (\\b -> getPatternVarName b.pattern) binds)
        newBound = Set.union bound bindNames
        bindVars = foldr (\\b s -> Set.union s (freeVarsInExprFor candidates newBound b.value)) Set.empty binds
    in Set.union bindVars (freeVarsInExprFor candidates newBound body)
  ExprCase scrut clauses ->
    let scrutVars = freeVarsInExprFor candidates bound scrut
        clauseVars = foldr (\\c s ->
          let clauseBound = addPatternVars c.pattern bound
              guardVars = case c.guard of
                Nothing -> Set.empty
                Just g -> freeVarsInExprFor candidates clauseBound g
              bodyVars = freeVarsInExprFor candidates clauseBound c.body
          in Set.union s (Set.union guardVars bodyVars)) Set.empty clauses
    in Set.union scrutVars clauseVars
  ExprIf cond thn els ->
    Set.union (freeVarsInExprFor candidates bound cond) (Set.union (freeVarsInExprFor candidates bound thn) (freeVarsInExprFor candidates bound els))
  ExprDo _ -> Set.empty
  ExprList exprs -> foldr (\\e s -> Set.union s (freeVarsInExprFor candidates bound e)) Set.empty exprs
  ExprTuple exprs -> foldr (\\e s -> Set.union s (freeVarsInExprFor candidates bound e)) Set.empty exprs
  ExprRecord fields -> foldr (\\(Tuple _ e) s -> Set.union s (freeVarsInExprFor candidates bound e)) Set.empty fields
  ExprRecordAccess e _ -> freeVarsInExprFor candidates bound e
  ExprRecordUpdate e updates -> Set.union (freeVarsInExprFor candidates bound e) (foldr (\\(Tuple _ v) s -> Set.union s (freeVarsInExprFor candidates bound v)) Set.empty updates)
  ExprTyped e _ -> freeVarsInExprFor candidates bound e
  where
    addPatternVars (PatVar n) s = Set.insert n s
    addPatternVars PatWildcard s = s
"""

tokens = Nova.Compiler.Tokenizer.tokenize(test_full)
IO.puts("Tokens: #{length(tokens)}")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("FAIL: #{inspect(err)}")
    # Binary search
    defmodule BS do
      def find(toks, lo, hi, last) when lo >= hi, do: last
      def find(toks, lo, hi, last) do
        mid = div(lo + hi, 2)
        case Nova.Compiler.Parser.parse_module(Enum.take(toks, mid)) do
          {:right, _} -> find(toks, mid + 1, hi, mid)
          {:left, _} -> find(toks, lo, mid, last)
        end
      end
    end
    lg = BS.find(tokens, 0, length(tokens), 0)
    IO.puts("Last good: #{lg}")
    for i <- [lg-1, lg, lg+1, lg+2, lg+3] do
      if i >= 0 && i < length(tokens) do
        t = Enum.at(tokens, i)
        IO.puts("  Token #{i}: '#{t.value}' (#{t.token_type}) line #{t.line}")
      end
    end
  {:right, {:tuple, mod, rest}} ->
    IO.puts("SUCCESS: #{length(mod.declarations)} decls")
end
