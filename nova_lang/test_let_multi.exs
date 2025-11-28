test = """
module Test where

foo expr = case expr of
  ExprLet binds body ->
    let bindNames = f binds
        newBound = g binds
    in h newBound body
  ExprCase scrut clauses ->
    f scrut
"""

tokens = Nova.Compiler.Tokenizer.tokenize(test)
IO.puts("Tokens: #{length(tokens)}")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} -> IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} -> IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end
