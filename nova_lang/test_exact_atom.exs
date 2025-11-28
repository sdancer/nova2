# Test the exact atom function from source

source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Extract lines 188-194 (0-indexed 187-193)
atom_lines = Enum.slice(lines, 187..193)
atom_code = "module Test where\n\n" <> Enum.join(atom_lines, "\n")

IO.puts("=== Exact atom code ===")
IO.puts(atom_code)
IO.puts("=== END ===")

tokens = Nova.Compiler.Tokenizer.tokenize(atom_code)
IO.puts("\nTokens: #{length(tokens)}")

result = case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} -> "FAIL: #{inspect(err)}"
  {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
end
IO.puts("Result: #{result}")

# Also test lines 186-194 (including comments)
atom_lines2 = Enum.slice(lines, 185..193)
atom_code2 = "module Test where\n\n" <> Enum.join(atom_lines2, "\n")

IO.puts("\n=== With comments ===")
IO.puts(atom_code2)
IO.puts("=== END ===")

tokens2 = Nova.Compiler.Tokenizer.tokenize(atom_code2)
result2 = case Nova.Compiler.Parser.parse_module(tokens2) do
  {:left, err} -> "FAIL: #{inspect(err)}"
  {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
end
IO.puts("Result: #{result2}")
