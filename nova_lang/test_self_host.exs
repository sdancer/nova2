# Test self-hosting: compile Ast.purs with the Elixir compiler and compare output
# Uses the Parser (non-CST) pipeline which is fully working

source = File.read!("../src/Nova/Compiler/Ast.purs")

# Tokenize and parse using the working Parser module
tokens = Nova.Compiler.Tokenizer.tokenize(source)
case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
    System.halt(1)
  {:right, {:tuple, ast, _remaining_tokens}} ->
    IO.puts("Parsed Ast.purs successfully!")

    # Generate code
    code = Nova.Compiler.CodeGen.gen_module(ast)
    lines = String.split(code, "\n") |> length()
    IO.puts("Generated #{lines} lines")

    # Compare with existing output
    existing = File.read!("lib/nova/compiler/Ast.ex")
    if code == existing do
      IO.puts("✓ OUTPUT MATCHES - Self-hosting verified for Ast.purs!")
    else
      IO.puts("✗ Output differs from existing file")
      # Show diff info
      existing_lines = String.split(existing, "\n")
      code_lines = String.split(code, "\n")
      IO.puts("Existing: #{length(existing_lines)} lines, Generated: #{length(code_lines)} lines")

      # Find first difference
      Enum.zip(existing_lines, code_lines)
      |> Enum.with_index(1)
      |> Enum.find(fn {{e, c}, _} -> e != c end)
      |> case do
        {{e, c}, line_num} ->
          IO.puts("\nFirst difference at line #{line_num}:")
          IO.puts("  Expected: #{String.slice(e, 0, 80)}")
          IO.puts("  Got:      #{String.slice(c, 0, 80)}")
        nil ->
          IO.puts("Files have different lengths but common lines match")
      end
    end
end
