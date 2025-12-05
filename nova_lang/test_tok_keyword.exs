# Test tok_keyword directly to see what's happening
tokens = [
  %{value: {:tok_lower_name, :nothing, "case"}, range: %{start: %{line: 1, column: 1}}}
]

IO.puts("Token value: #{inspect(hd(tokens).value)}")

# Test satisfy directly
is_kw = fn auto_arg0 ->
  IO.puts("satisfy checking: #{inspect(auto_arg0)}")
  case auto_arg0 do
    ({:tok_lower_name, :nothing, name}) ->
      IO.puts("  Matched pattern, name=#{inspect(name)}, checking name == 'case': #{name == "case"}")
      (name == "case")
    _ ->
      IO.puts("  Did not match pattern")
      false
  end
end

# Call satisfy manually
IO.puts("\nManual satisfy test on token value:")
result = is_kw.(hd(tokens).value)
IO.puts("Result: #{result}")

IO.puts("\n\nNow testing via CstParser.tok_keyword:")
parser = Nova.Compiler.CstParser.tok_keyword("case")
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(tokens)
IO.inspect(result, label: "tok_keyword result")

# Let's also check what satisfy does
IO.puts("\n\nChecking satisfy implementation:")
# satisfy takes a predicate and returns a parser
# The parser should check tokens[0].value against the predicate

# What if the issue is that satisfy is checking the whole token, not .value?
IO.puts("\nTest: What if satisfy checks whole token instead of .value?")
is_kw2 = fn token ->
  IO.puts("satisfy checking whole token: #{inspect(token)}")
  case token do
    %{value: {:tok_lower_name, :nothing, name}} ->
      IO.puts("  Matched %{value: pattern}, name=#{inspect(name)}")
      (name == "case")
    ({:tok_lower_name, :nothing, name}) ->
      IO.puts("  Matched tuple pattern, name=#{inspect(name)}")
      (name == "case")
    _ ->
      IO.puts("  Did not match any pattern")
      false
  end
end

IO.puts("\nTest on whole token:")
is_kw2.(hd(tokens))
IO.puts("\nTest on token.value:")
is_kw2.(hd(tokens).value)
