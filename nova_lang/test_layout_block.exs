# Test layout_block directly
# Tokens after "case x of" are:
# {:tok_layout_start, 5}
# {:tok_upper_name, :nothing, "Nothing"}
# :tok_right_arrow
# {:tok_int, "1", {:small_int, 1}}
# {:tok_layout_sep, 5}
# {:tok_upper_name, :nothing, "Just"}
# {:tok_lower_name, :nothing, "y"}
# :tok_right_arrow
# {:tok_lower_name, :nothing, "y"}
# {:tok_layout_end, 5}

# Let's create these tokens manually
tokens = [
  %{value: {:tok_layout_start, 5}, range: %{start: %{line: 4, column: 5}}},
  %{value: {:tok_upper_name, :nothing, "Nothing"}, range: %{start: %{line: 4, column: 5}}},
  %{value: :tok_right_arrow, range: %{start: %{line: 4, column: 13}}},
  %{value: {:tok_int, "1", {:small_int, 1}}, range: %{start: %{line: 4, column: 16}}},
  %{value: {:tok_layout_sep, 5}, range: %{start: %{line: 5, column: 5}}},
  %{value: {:tok_upper_name, :nothing, "Just"}, range: %{start: %{line: 5, column: 5}}},
  %{value: {:tok_lower_name, :nothing, "y"}, range: %{start: %{line: 5, column: 10}}},
  %{value: :tok_right_arrow, range: %{start: %{line: 5, column: 12}}},
  %{value: {:tok_lower_name, :nothing, "y"}, range: %{start: %{line: 5, column: 15}}},
  %{value: {:tok_layout_end, 5}, range: %{start: %{line: 6, column: 1}}}
]

IO.puts("Testing tok_layout_start:")
parser = Nova.Compiler.CstParser.tok_layout_start()
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(tokens)
IO.inspect(result, label: "tok_layout_start result")

IO.puts("\n\nTesting parse_binder:")
binder_tokens = [
  %{value: {:tok_upper_name, :nothing, "Nothing"}, range: %{start: %{line: 4, column: 5}}}
]
parser = Nova.Compiler.CstParser.parse_binder()
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(binder_tokens)
IO.inspect(result, label: "parse_binder result", limit: 5)

IO.puts("\n\nTesting parse_case_branch:")
branch_tokens = [
  %{value: {:tok_upper_name, :nothing, "Nothing"}, range: %{start: %{line: 4, column: 5}}},
  %{value: :tok_right_arrow, range: %{start: %{line: 4, column: 13}}},
  %{value: {:tok_int, "1", {:small_int, 1}}, range: %{start: %{line: 4, column: 16}}}
]
parser = Nova.Compiler.CstParser.parse_case_branch()
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(branch_tokens)
IO.inspect(result, label: "parse_case_branch result", limit: 5)

IO.puts("\n\nTesting layout_block(parse_case_branch):")
parser = Nova.Compiler.CstParser.layout_block(Nova.Compiler.CstParser.parse_case_branch())
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(tokens)
IO.inspect(result, label: "layout_block result", limit: 5)
