# Test the last part of CodeGenCoreErlang.purs as standalone

test_end = """
module Test where

translateOp op = op

toSnakeCase s =
  let chars = toCharArray s
  in fromCharArray (Array.concatMap convertChar chars)
  where
    convertChar c =
      if isUpper c
      then ['_', toLower c]
      else [c]
    isUpper c = c >= 'A' && c <= 'Z'
    toLower c =
      fromMaybe c (charAt 0 (toLower (singleton c)))

genCoreList ctx elems =
  if Array.length elems <= 50
  then "[" <> intercalate ", " (map (genExpr ctx) elems) <> "]"
  else case Array.uncons elems of
    Nothing -> "[]"
    Just { head: h, tail: [] } -> "[" <> genExpr ctx h <> "]"
    Just { head: h, tail: t } -> "[" <> genExpr ctx h <> "|" <> genCoreList ctx t <> "]"

collectArgs expr = go expr []
  where
    go (ExprApp f a) acc = go f (a : acc)
    go f acc = { func: f, args: acc }

lookupArity name ctx =
  case Array.find (\\f -> f.name == name) ctx.funcArities of
    Just f -> f.arity
    Nothing -> 0
"""

tokens = Nova.Compiler.Tokenizer.tokenize(test_end)
IO.puts("Tokens: #{length(tokens)}")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} -> IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} -> IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end
