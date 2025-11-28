source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")
total = length(lines)
IO.puts("Total lines: #{total}")

for n <- [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, total] do
  code = Enum.take(lines, n) |> Enum.join("\n")
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, _} -> "FAIL"
    {:right, {:tuple, mod, _}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("Lines 1-#{n}: #{result}")
end
