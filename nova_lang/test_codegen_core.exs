source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)
IO.puts("Total tokens: #{length(tokens)}")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} ->
    IO.puts("SUCCESS!")
    IO.puts("Module: #{mod.name}")
    IO.puts("Declarations: #{length(mod.declarations)}")
    IO.puts("Remaining tokens: #{length(rest)}")
end
