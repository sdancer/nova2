mod = "Tokenizer"
source = File.read!("../src/Nova/Compiler/#{mod}.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)
case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
  {:right, {:tuple, ast, _}} ->
    code = Nova.Compiler.CodeGen.gen_module(ast)
    File.write!("/tmp/generated_#{mod}.ex", code)
    IO.puts("Generated to /tmp/generated_#{mod}.ex")
end
