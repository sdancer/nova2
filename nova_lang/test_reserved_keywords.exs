# Test if the reserved_keywords list is properly formed
# The PureScript cons : should become proper Elixir lists

IO.puts("Testing reserved_keywords list structure:")

# This is what the compiler generates:
reserved_keywords = [[[[[[[[[[[[[[[[[[[[[[[[[[["module" | "where"] | "import"] | "data"] | "type"] | "newtype"] | "class"] | "instance"] | "derive"] | "foreign"] | "infixl"] | "infixr"] | "infix"] | "if"] | "then"] | "else"] | "case"] | "of"] | "let"] | "in"] | "do"] | "ado"] | "forall"] | "as"] | "hiding"] | "true"] | "false"] | []]

IO.puts("reserved_keywords type: #{inspect(reserved_keywords)}")
IO.puts("Is list? #{is_list(reserved_keywords)}")

# Check if elem works
IO.puts("\nTrying Nova.List.elem('of', reserved_keywords):")
result = Nova.List.elem("of", reserved_keywords)
IO.puts("Result: #{result}")

# What it should be (proper list):
proper_list = ["module", "where", "import", "data", "type", "newtype", "class", "instance", "derive", "foreign", "infixl", "infixr", "infix", "if", "then", "else", "case", "of", "let", "in", "do", "ado", "forall", "as", "hiding", "true", "false"]
IO.puts("\nProper list: #{inspect(proper_list)}")

IO.puts("\nChecking 'of' in proper list: #{Enum.member?(proper_list, "of")}")

# Test the tok_qualified_lower_name function directly
IO.puts("\n\nTest tok_qualified_lower_name on 'of' token:")
tokens = [
  %{value: {:tok_lower_name, :nothing, "of"}, range: %{start: %{line: 1, column: 1}}}
]

parser = Nova.Compiler.CstParser.tok_qualified_lower_name()
parser_fn = Nova.Compiler.CstParser.run_parser(parser)
result = parser_fn.(tokens)

case result do
  {:right, _} -> IO.puts("PROBLEM: tok_qualified_lower_name matched 'of' - should have been rejected!")
  {:left, _} -> IO.puts("CORRECT: tok_qualified_lower_name rejected 'of' as expected")
end

# Test on a normal identifier
IO.puts("\n\nTest tok_qualified_lower_name on 'x' token:")
tokens2 = [
  %{value: {:tok_lower_name, :nothing, "x"}, range: %{start: %{line: 1, column: 1}}}
]
result2 = parser_fn.(tokens2)

case result2 do
  {:right, _} -> IO.puts("CORRECT: tok_qualified_lower_name matched 'x'")
  {:left, _} -> IO.puts("PROBLEM: tok_qualified_lower_name rejected 'x'")
end
