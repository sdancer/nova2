# Test parse_expr5 which is the top-level expression parser that uses alt to try different options
# The issue is parse_case failing when called from parse_expr5's alt chain

code = """
foo x =
  case x of
    Nothing -> 1
    Just y -> y
"""

tokens = Nova.Compiler.CstLexer.lex_module(code)

IO.puts("All tokens:")
tokens
|> Enum.with_index()
|> Enum.each(fn {tok, i} ->
  IO.puts("  #{i}: #{inspect(tok.value)}")
end)

# The expression tokens should start at "case" (after "foo x =")
# Skip: foo, x, =
case_tokens = tokens |> Enum.drop(3)

IO.puts("\n\nTokens starting from 'case':")
case_tokens
|> Enum.with_index()
|> Enum.each(fn {tok, i} ->
  IO.puts("  #{i}: #{inspect(tok.value)}")
end)

# First, let's test tok_keyword on the first token directly
IO.puts("\n\nTest tok_keyword('case') on case_tokens:")
parser = Nova.Compiler.CstParser.tok_keyword("case")
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(case_tokens)
IO.inspect(result, label: "tok_keyword result", limit: 5)

# Now test parse_case
IO.puts("\n\nTest parse_case() on case_tokens:")
parser = Nova.Compiler.CstParser.parse_case()
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(case_tokens)
case result do
  {:right, {:tuple, expr, remaining}} ->
    IO.puts("Success!")
    IO.puts("Remaining tokens: #{length(remaining)}")
  {:left, err} ->
    IO.puts("Error: #{err}")
end

# Now test parse_expr5 (the top-level expr parser)
IO.puts("\n\nTest parse_expr5() on case_tokens:")
parser = Nova.Compiler.CstParser.parse_expr5()
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(case_tokens)
case result do
  {:right, {:tuple, expr, remaining}} ->
    IO.puts("Success! Parsed: #{elem(expr, 0)}")
    IO.puts("Remaining tokens: #{length(remaining)}")
  {:left, err} ->
    IO.puts("Error: #{err}")
end
