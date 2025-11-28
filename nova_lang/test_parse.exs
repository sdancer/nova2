# Test if parsing works with both tokenizers
src = File.read!("../src/Nova/Compiler/Types.purs")

IO.puts("Tokenizing with fast tokenizer...")
fast_tokens = Nova.FastTokenizer.tokenize(src)
IO.puts("Got #{length(fast_tokens)} tokens")

IO.puts("\nParsing with fast tokens...")
case Nova.Compiler.Parser.parse_module(fast_tokens) do
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} ->
    IO.puts("Success! Module: #{mod.name}")
    IO.puts("Remaining tokens: #{length(rest)}")
end
