source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)
IO.puts("Tokens: #{length(tokens)}")
case Nova.Compiler.Parser.parse_module(tokens) do
  {:_left, err} -> IO.puts("Parse error: #{inspect(err)}")
  {:_right, {:_tuple, mod, _}} -> IO.puts("Parsed module: #{mod.name}")
end
