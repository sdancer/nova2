# Test what happens with preceding code

source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

IO.puts("Testing lines 1-N from CodeGenCoreErlang.purs:")

for n <- [145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 160, 165, 170] do
  code = Enum.take(lines, n) |> Enum.join("\n")
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} ->
      # Get first 50 chars of error
      "FAIL: #{String.slice(inspect(err), 0, 50)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("Lines 1-#{n}: #{result}")
end

# Show the critical lines 145-155
IO.puts("\n=== Lines 145-155 ===")
for {line, i} <- Enum.with_index(Enum.slice(lines, 144..154), 145) do
  IO.puts("#{i}: #{String.slice(line, 0, 80)}")
end
