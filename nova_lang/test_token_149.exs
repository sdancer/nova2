source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Lines 1-149 (known to work with 46 decls)
code_149 = Enum.take(lines, 149) |> Enum.join("\n")
tokens_149 = Nova.Compiler.Tokenizer.tokenize(code_149)
IO.puts("Lines 1-149: #{length(tokens_149)} tokens")

case Nova.Compiler.Parser.parse_module(tokens_149) do
  {:left, err} -> IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} -> IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end

# Lines 1-150
code_150 = Enum.take(lines, 150) |> Enum.join("\n")
tokens_150 = Nova.Compiler.Tokenizer.tokenize(code_150)
IO.puts("\nLines 1-150: #{length(tokens_150)} tokens")

case Nova.Compiler.Parser.parse_module(tokens_150) do
  {:left, err} -> IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} -> IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end

# Try parsing first (tokens_149 + 1) tokens from 150-line file
n = length(tokens_149)
IO.puts("\nFirst #{n} tokens from lines 1-150:")
case Nova.Compiler.Parser.parse_module(Enum.take(tokens_150, n)) do
  {:left, err} -> IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} -> IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end

# Show the tokens around the boundary
IO.puts("\nLine 149 content: #{Enum.at(lines, 148)}")
IO.puts("Line 150 content: #{Enum.at(lines, 149)}")

IO.puts("\nTokens #{n-5} to #{n+10}:")
for i <- (n-5)..(n+10) do
  if i >= 0 && i < length(tokens_150) do
    t = Enum.at(tokens_150, i)
    IO.puts("  #{i}: #{t.token_type} '#{String.slice(to_string(t.value), 0, 30)}' line #{t.line}")
  end
end
