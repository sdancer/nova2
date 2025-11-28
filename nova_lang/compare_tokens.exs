# Compare tokens from slow vs fast tokenizer
src = File.read!("../src/Nova/Compiler/Types.purs")

slow = Nova.Compiler.Tokenizer.tokenize(src) |> Enum.take(30)
fast = Nova.FastTokenizer.tokenize(src) |> Enum.take(30)

IO.puts("Slow tokens:")
for t <- slow do
  IO.inspect(t)
end

IO.puts("\nFast tokens:")
for t <- fast do
  IO.inspect(t)
end

IO.puts("\nToken count: slow=#{length(Nova.Compiler.Tokenizer.tokenize(src))} fast=#{length(Nova.FastTokenizer.tokenize(src))}")
