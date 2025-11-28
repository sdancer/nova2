source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")
total_lines = length(lines)
IO.puts("Total lines: #{total_lines}")

# Test a sampling of lines
sample = [50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200]

for n <- sample do
  code = Enum.take(lines, n) |> Enum.join("\n")
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, _} -> "FAIL"
    {:right, {:tuple, mod, _}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("Lines 1-#{n}: #{result}")
end
