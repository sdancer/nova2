src = File.read!("test/purs/SimpleTest.purs")
tokens = Nova.FastTokenizer.tokenize(src)

case Nova.Compiler.Parser.parse_module(tokens) do
  {:right, {:tuple, mod, _}} ->
    IO.puts("Module: #{mod.name}")
    IO.puts("Declarations:")
    Enum.each(mod.declarations, fn decl ->
      IO.inspect(decl, label: "  Decl", pretty: true, limit: :infinity)
    end)

  {:left, err} ->
    IO.puts("Parse error: #{err}")
end
