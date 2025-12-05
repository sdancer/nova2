# Test separated(parse_expr(), tok_comma()) on case expression
# After parsing "case", the next tokens are: x of <layout_start> Nothing -> ...

case_head_tokens = [
  %{value: {:tok_lower_name, :nothing, "x"}, range: %{start: %{line: 1, column: 6}}},
  %{value: {:tok_lower_name, :nothing, "of"}, range: %{start: %{line: 1, column: 8}}},
  %{value: {:tok_layout_start, 5}, range: %{start: %{line: 2, column: 5}}}
]

IO.puts("Tokens for case head:")
case_head_tokens
|> Enum.with_index()
|> Enum.each(fn {tok, i} ->
  IO.puts("  #{i}: #{inspect(tok.value)}")
end)

IO.puts("\n\nTest separated(parse_expr(), tok_comma()):")
parser = Nova.Compiler.CstParser.separated(Nova.Compiler.CstParser.parse_expr(), Nova.Compiler.CstParser.tok_comma())
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(case_head_tokens)

case result do
  {:right, {:tuple, exprs, remaining}} ->
    IO.puts("Success!")
    IO.puts("Parsed exprs type: #{inspect(elem(exprs, 0))}")
    IO.puts("Remaining tokens: #{length(remaining)}")
    remaining |> Enum.each(fn tok ->
      IO.puts("  #{inspect(tok.value)}")
    end)
  {:left, err} ->
    IO.puts("Error: #{err}")
end

IO.puts("\n\n=== Now test full parse_case on full tokens ===")

# Full case tokens
full_case_tokens = [
  %{value: {:tok_lower_name, :nothing, "case"}, range: %{start: %{line: 1, column: 1}}},
  %{value: {:tok_lower_name, :nothing, "x"}, range: %{start: %{line: 1, column: 6}}},
  %{value: {:tok_lower_name, :nothing, "of"}, range: %{start: %{line: 1, column: 8}}},
  %{value: {:tok_layout_start, 5}, range: %{start: %{line: 2, column: 5}}},
  %{value: {:tok_upper_name, :nothing, "Nothing"}, range: %{start: %{line: 2, column: 5}}},
  %{value: :tok_right_arrow, range: %{start: %{line: 2, column: 13}}},
  %{value: {:tok_int, "1", {:small_int, 1}}, range: %{start: %{line: 2, column: 16}}},
  %{value: {:tok_layout_end, 5}, range: %{start: %{line: 3, column: 1}}}
]

IO.puts("\nTest tok_keyword('case'):")
parser = Nova.Compiler.CstParser.tok_keyword("case")
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(full_case_tokens)
case result do
  {:right, {:tuple, tok, remaining}} ->
    IO.puts("Success! Consumed 'case'")
    IO.puts("Next token: #{inspect(hd(remaining).value)}")
  {:left, err} ->
    IO.puts("Error: #{err}")
end

IO.puts("\n\nTest after 'case' -> separated(parse_expr, tok_comma):")
after_case = Enum.drop(full_case_tokens, 1)
parser = Nova.Compiler.CstParser.separated(Nova.Compiler.CstParser.parse_expr(), Nova.Compiler.CstParser.tok_comma())
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(after_case)
case result do
  {:right, {:tuple, exprs, remaining}} ->
    IO.puts("Success!")
    IO.puts("Exprs: #{inspect(exprs, limit: 3)}")
    IO.puts("Remaining tokens: #{length(remaining)}")
    remaining |> Enum.take(3) |> Enum.each(fn tok ->
      IO.puts("  #{inspect(tok.value)}")
    end)
  {:left, err} ->
    IO.puts("Error: #{err}")
end
