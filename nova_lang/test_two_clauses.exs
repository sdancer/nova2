test = """
module Test where
getPreludeFunc "map" = Just { mod: "functor", func: "map", arity: 2 }
getPreludeFunc "filter" = Just { mod: "lists", func: "filter", arity: 2 }
"""

tokens = Nova.Compiler.Tokenizer.tokenize(test)
IO.puts("Tokens: #{length(tokens)}")
# for t <- tokens do
#   IO.puts("  #{t.token_type}: '#{t.value}'")
# end

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} -> IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} ->
    IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
    for d <- mod.declarations do
      case d do
        {:decl_function, f} -> IO.puts("  Function: #{f.name}")
        _ -> IO.puts("  Other: #{inspect(d)}")
      end
    end
end
