# Test tokenizing progressively larger chunks
src = File.read!("../src/Nova/Compiler/Types.purs")
lines = String.split(src, "\n")
IO.puts("Total lines: #{length(lines)}")

# Try first 100 lines
chunk = Enum.take(lines, 100) |> Enum.join("\n")
IO.puts("Testing first 100 lines (#{String.length(chunk)} chars)...")
t1 = System.monotonic_time(:millisecond)
tokens = Nova.Compiler.Tokenizer.tokenize(chunk)
t2 = System.monotonic_time(:millisecond)
IO.puts("  Got #{length(tokens)} tokens in #{t2 - t1}ms")

# Try first 200 lines
chunk = Enum.take(lines, 200) |> Enum.join("\n")
IO.puts("Testing first 200 lines (#{String.length(chunk)} chars)...")
t1 = System.monotonic_time(:millisecond)
tokens = Nova.Compiler.Tokenizer.tokenize(chunk)
t2 = System.monotonic_time(:millisecond)
IO.puts("  Got #{length(tokens)} tokens in #{t2 - t1}ms")

# Try first 400 lines
chunk = Enum.take(lines, 400) |> Enum.join("\n")
IO.puts("Testing first 400 lines (#{String.length(chunk)} chars)...")
t1 = System.monotonic_time(:millisecond)
tokens = Nova.Compiler.Tokenizer.tokenize(chunk)
t2 = System.monotonic_time(:millisecond)
IO.puts("  Got #{length(tokens)} tokens in #{t2 - t1}ms")
