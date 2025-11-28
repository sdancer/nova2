source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Test around line 1210
for n <- [1200, 1205, 1208, 1209, 1210, 1211, 1212, 1215, 1220] do
  code = Enum.take(lines, n) |> Enum.join("\n")
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, _} -> "FAIL"
    {:right, {:tuple, mod, _}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("Lines 1-#{n}: #{result}")
end
