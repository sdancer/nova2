# Test exact source from CodeGenCoreErlang.purs

source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Test 1: Just topoSortBinds function (lines 149-184)
topo_lines = Enum.slice(lines, 148..183)  # 0-indexed
topo_code = "module Test where\n\n" <> Enum.join(topo_lines, "\n")

IO.puts("=== topoSortBinds code ===")
IO.puts(String.slice(topo_code, 0, 500))
IO.puts("...")
IO.puts("=== END ===")

tokens = Nova.Compiler.Tokenizer.tokenize(topo_code)
IO.puts("\nTokens: #{length(tokens)}")

result = case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} -> "FAIL: #{inspect(err)}"
  {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
end
IO.puts("Result: #{result}")

# If it fails, try progressively smaller slices
if String.starts_with?(result, "FAIL") do
  IO.puts("\n--- Binary search for failure point ---")

  for end_line <- [155, 160, 165, 170, 175, 180] do
    slice_lines = Enum.slice(lines, 148..end_line)
    slice_code = "module Test where\n\n" <> Enum.join(slice_lines, "\n")
    tokens = Nova.Compiler.Tokenizer.tokenize(slice_code)
    result = case Nova.Compiler.Parser.parse_module(tokens) do
      {:left, _} -> "FAIL"
      {:right, {:tuple, mod, _}} -> "OK #{length(mod.declarations)} decls"
    end
    IO.puts("Lines 149-#{end_line + 1}: #{result}")
  end
end
