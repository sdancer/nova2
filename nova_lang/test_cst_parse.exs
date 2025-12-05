# Test to see what happens when parsing case expression
code = """
module Test where

foo x =
  case x of
    Nothing -> 1
    Just y -> y

bar = 1
"""

tokens = Nova.Compiler.CstLexer.lex_module(code)

IO.puts("Total tokens: #{length(tokens)}")

# Try parsing
parser = Nova.Compiler.CstParser.parse_module()
parser_fn = Nova.Compiler.CstParser.run_parser(parser)

result = parser_fn.(tokens)

case result do
  {:right, {:tuple, cst_module, remaining}} ->
    IO.puts("Parse succeeded!")
    IO.puts("Remaining tokens: #{length(remaining)}")
    decls = cst_module.body.decls
    IO.puts("Number of declarations: #{length(decls)}")
    Enum.with_index(decls) |> Enum.each(fn {d, i} ->
      IO.puts("  #{i}: #{elem(d, 0)}")
    end)

    if length(remaining) > 0 do
      IO.puts("\nRemaining tokens:")
      Enum.each(remaining, fn tok ->
        IO.puts("  #{inspect(tok.value)}")
      end)
    end

  {:left, err} ->
    IO.puts("Parse error: #{err}")
end
