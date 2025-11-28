# Test exact pattern from CodeGenCoreErlang.purs lines 1204-1214

test_where = """
module Test where

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
"""

tokens = Nova.Compiler.Tokenizer.tokenize(test_where)
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
    for i <- [lg-1, lg, lg+1, lg+2] do
      if i >= 0 && i < length(tokens) do
        t = Enum.at(tokens, i)
        IO.puts("  Token #{i}: '#{t.value}' (#{t.token_type}) line #{t.line}")
      end
    end
  {:right, {:tuple, mod, rest}} ->
    IO.puts("OK: #{length(mod.declarations)} decls")
end
