mod = System.argv() |> List.first() || "Types"
source = File.read!("../src/Nova/Compiler/#{mod}.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)
case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
  {:right, {:tuple, ast, _}} ->
    code = Nova.Compiler.CodeGen.gen_module(ast)
    File.write!("generated_#{mod}.ex", code)
    # Run diff
    System.cmd("diff", ["lib/nova/compiler/#{mod}.ex", "generated_#{mod}.ex"])
    |> elem(0)
    |> String.split("\n")
    |> Enum.take(100)
    |> Enum.join("\n")
    |> IO.puts()
end
