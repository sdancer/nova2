# Debug token differences between slow and fast tokenizer

# Read a small sample
src = """
module Test where

import Prelude

foo :: Int -> Int
foo x = x + 1
"""

slow = Nova.Compiler.Tokenizer.tokenize(src)
fast = Nova.FastTokenizer.tokenize(src)

IO.puts("Slow tokenizer: #{length(slow)} tokens")
IO.puts("Fast tokenizer: #{length(fast)} tokens")
IO.puts("")

IO.puts("Comparing tokens...")
Enum.zip(slow, fast)
|> Enum.with_index()
|> Enum.each(fn {{s, f}, idx} ->
  # Compare just the key parts
  diff = s.token_type != f.token_type or s.value != f.value or s.line != f.line
  if diff do
    IO.puts("Token #{idx} differs:")
    IO.puts("  Slow: type=#{inspect(s.token_type)} val=#{inspect(s.value)} L#{s.line}:#{s.column} pos=#{s.pos}")
    IO.puts("  Fast: type=#{inspect(f.token_type)} val=#{inspect(f.value)} L#{f.line}:#{f.column} pos=#{f.pos}")
  end
end)

if length(slow) != length(fast) do
  IO.puts("")
  IO.puts("Different lengths! slow=#{length(slow)} fast=#{length(fast)}")
  if length(slow) > length(fast) do
    IO.puts("Extra slow tokens:")
    Enum.drop(slow, length(fast)) |> Enum.each(&IO.inspect/1)
  else
    IO.puts("Extra fast tokens:")
    Enum.drop(fast, length(slow)) |> Enum.each(&IO.inspect/1)
  end
end
