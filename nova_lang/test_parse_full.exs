source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)
IO.puts("Total tokens: #{length(tokens)}")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("PARSE ERROR: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} ->
    IO.puts("OK: #{length(mod.declarations)} declarations")
    IO.puts("Remaining tokens: #{length(rest)}")
    if length(rest) > 0 do
      IO.puts("First remaining: #{inspect(Enum.take(rest, 5))}")
    end
end
