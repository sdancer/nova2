source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Compare tokens from lines 1-50 vs lines 1-150

code_50 = Enum.take(lines, 50) |> Enum.join("\n")
tokens_50 = Nova.Compiler.Tokenizer.tokenize(code_50)
IO.puts("Lines 1-50: #{length(tokens_50)} tokens")

code_150 = Enum.take(lines, 150) |> Enum.join("\n")
tokens_150 = Nova.Compiler.Tokenizer.tokenize(code_150)
IO.puts("Lines 1-150: #{length(tokens_150)} tokens")

# Show tokens 360-375 from the 150-line tokenization
IO.puts("\nTokens 360-375 from lines 1-150:")
for i <- 360..375 do
  t = Enum.at(tokens_150, i)
  if t do
    IO.puts("  #{i}: #{t.token_type} '#{String.slice(to_string(t.value), 0, 20)}' line #{t.line}")
  end
end

# Show tokens 360-375 from the 50-line tokenization
IO.puts("\nTokens 360-375 from lines 1-50:")
for i <- 360..375 do
  t = Enum.at(tokens_50, i)
  if t do
    IO.puts("  #{i}: #{t.token_type} '#{String.slice(to_string(t.value), 0, 20)}' line #{t.line}")
  end
end

# Are they the same?
IO.puts("\nComparing first #{length(tokens_50)} tokens:")
diffs = for i <- 0..(length(tokens_50)-1) do
  t50 = Enum.at(tokens_50, i)
  t150 = Enum.at(tokens_150, i)
  if t50.value != t150.value || t50.token_type != t150.token_type do
    IO.puts("  Diff at #{i}: 50='#{t50.value}' 150='#{t150.value}'")
    1
  else
    0
  end
end
IO.puts("Total diffs: #{Enum.sum(diffs)}")
