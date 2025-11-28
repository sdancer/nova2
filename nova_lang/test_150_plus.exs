source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

for n <- [150, 155, 160, 165, 166, 167, 168, 169, 170, 175, 180, 185, 190, 200] do
  code = Enum.take(lines, n) |> Enum.join("\n")
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, _} -> "FAIL"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("Lines 1-#{n}: #{result}")
end

# Show lines 165-170
IO.puts("\nLines 165-170 content:")
for n <- 165..170 do
  IO.puts("  #{n}: #{Enum.at(lines, n - 1)}")
end
