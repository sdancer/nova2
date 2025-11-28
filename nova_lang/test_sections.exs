src = File.read!("test/purs/SectionTest.purs")
tokens = Nova.FastTokenizer.tokenize(src)
IO.puts("Tokens: #{length(tokens)}")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:right, {:tuple, mod, remaining}} ->
    IO.puts("Module: #{mod.name}")
    IO.puts("Declarations: #{length(mod.declarations)}")
    IO.puts("Remaining: #{length(remaining)}")

    code = Nova.Compiler.CodeGen.gen_module(mod)
    IO.puts("\n--- Generated Elixir ---")
    IO.puts(code)

  {:left, err} ->
    IO.puts("Parse error: #{err}")
end
