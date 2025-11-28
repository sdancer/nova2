# Exact indentation from the original file

test1 = """
module Test where

foo expr = case expr of
  ExprLet binds body ->
    let bindNames = f binds
        newBound = g binds
    in h newBound body
  ExprCase scrut clauses ->
    f scrut
"""

test2 = """
module Test where

foo expr = case expr of
  ExprLet binds body ->
    let bindNames = f binds
    in h bindNames body
  ExprCase scrut clauses ->
    f scrut
"""

test3 = """
module Test where

foo expr = case expr of
  ExprLet binds body ->
    let x = f binds
        y = g binds
        z = h binds
    in combine x y z
  ExprCase scrut clauses ->
    process scrut
  ExprIf cond thn els ->
    branch cond thn els
"""

for {label, code} <- [{"multi-bind let", test1}, {"single-bind let", test2}, {"3 clauses", test3}] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 60)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("#{label}: #{result}")
end
