source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Narrow down between 100-150
for n <- [100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150] do
  code = Enum.take(lines, n) |> Enum.join("\n")
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, _} -> "FAIL"
    {:right, {:tuple, mod, _}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("Lines 1-#{n}: #{result}")
end
