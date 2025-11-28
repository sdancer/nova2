# Debug token differences for Types.purs
src = File.read!("../src/Nova/Compiler/Types.purs")

IO.puts("Tokenizing...")
slow = Nova.Compiler.Tokenizer.tokenize(src)
fast = Nova.FastTokenizer.tokenize(src)

IO.puts("Slow: #{length(slow)} tokens")
IO.puts("Fast: #{length(fast)} tokens")

# Find first difference
IO.puts("\nFinding first significant difference...")
min_len = min(length(slow), length(fast))

# Compare tokens
Enum.zip(Enum.take(slow, min_len), Enum.take(fast, min_len))
|> Enum.with_index()
|> Enum.find(fn {{s, f}, _idx} ->
  s.token_type != f.token_type or s.value != f.value
end)
|> case do
  nil ->
    IO.puts("No token type/value differences found in first #{min_len} tokens")
    if length(slow) != length(fast) do
      IO.puts("But lengths differ!")
    end
  {{s, f}, idx} ->
    IO.puts("First difference at token #{idx}:")
    IO.puts("  Slow: type=#{inspect(s.token_type)} val=#{inspect(s.value)}")
    IO.puts("  Fast: type=#{inspect(f.token_type)} val=#{inspect(f.value)}")
    # Show context
    IO.puts("\nContext (5 tokens before):")
    Enum.slice(slow, max(0, idx-5), 5) |> Enum.each(fn t ->
      IO.puts("  #{inspect(t.token_type)} #{inspect(t.value)}")
    end)
end

# Test parsing with slow tokens
IO.puts("\n\nParsing with SLOW tokens...")
case Nova.Compiler.Parser.parse_module(slow) do
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} ->
    IO.puts("Success! Module: #{mod.name}, remaining: #{length(rest)}")
end
