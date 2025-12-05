# Test parsing just a case expression
code = """
foo x =
  case x of
    Nothing -> 1
    Just y -> y
"""

tokens = Nova.Compiler.CstLexer.lex_module(code)

IO.puts("Tokens:")
tokens
|> Enum.with_index()
|> Enum.each(fn {tok, i} ->
  IO.puts("  #{i}: #{inspect(tok.value)}")
end)

# Try parsing just parse_case (without module wrapper)
# Need to start after 'foo x ='
# That would be tokens 3 onwards

# Actually let's check the exact case-parsing portion
# Tokens are: foo x = case x of <layout_start> Nothing -> 1 <sep> Just y -> y <layout_end>

case_tokens = tokens |> Enum.drop(3)  # Skip 'foo x ='

IO.puts("\n\nCase tokens starting from 'case':")
case_tokens
|> Enum.with_index()
|> Enum.each(fn {tok, i} ->
  IO.puts("  #{i}: #{inspect(tok.value)}")
end)

IO.puts("\n\nTrying parse_case():")
parser = Nova.Compiler.CstParser.parse_case()
parser_fn = Nova.Compiler.CstParser.run_parser(parser)

result = parser_fn.(case_tokens)

case result do
  {:right, {:tuple, expr, remaining}} ->
    IO.puts("Success! Parsed case expression")
    IO.puts("Remaining tokens: #{length(remaining)}")
    remaining |> Enum.take(5) |> Enum.each(fn tok ->
      IO.puts("  #{inspect(tok.value)}")
    end)

  {:left, err} ->
    IO.puts("Parse error: #{err}")
end
