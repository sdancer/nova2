source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)
IO.puts("Tokens: #{length(tokens)}")

result = Nova.Compiler.Parser.parse_module(tokens)
case result do
  {:left, err} -> IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} -> IO.puts("OK: #{length(mod.declarations)} declarations")
end
