# Test with exact lines from CodeGenCoreErlang.purs

source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# First test: lines 1-125 (stop just before the failing line)
test_125 = Enum.take(lines, 125) |> Enum.join("\n")
tokens_125 = Nova.Compiler.Tokenizer.tokenize(test_125)

case Nova.Compiler.Parser.parse_module(tokens_125) do
  {:left, err} -> IO.puts("Lines 1-125: FAIL: #{String.slice(inspect(err), 0, 80)}")
  {:right, {:tuple, mod, _}} -> IO.puts("Lines 1-125: OK #{length(mod.declarations)} decls")
end

# Second test: lines 1-130
test_130 = Enum.take(lines, 130) |> Enum.join("\n")
tokens_130 = Nova.Compiler.Tokenizer.tokenize(test_130)

case Nova.Compiler.Parser.parse_module(tokens_130) do
  {:left, err} -> IO.puts("Lines 1-130: FAIL: #{String.slice(inspect(err), 0, 80)}")
  {:right, {:tuple, mod, _}} -> IO.puts("Lines 1-130: OK #{length(mod.declarations)} decls")
end

# Find exact failing line
IO.puts("\nSearching for exact failing line...")

defmodule LineSearch do
  def find(lines, lo, hi, last) when lo >= hi, do: last
  def find(lines, lo, hi, last) do
    mid = div(lo + hi, 2)
    code = Enum.take(lines, mid) |> Enum.join("\n")
    tokens = Nova.Compiler.Tokenizer.tokenize(code)
    case Nova.Compiler.Parser.parse_module(tokens) do
      {:right, _} -> find(lines, mid + 1, hi, mid)
      {:left, _} -> find(lines, lo, mid, last)
    end
  end
end

last_good_line = LineSearch.find(lines, 1, length(lines), 1)
IO.puts("Last good line: #{last_good_line}")

# Show context
IO.puts("\nLines around failure:")
for n <- [last_good_line - 1, last_good_line, last_good_line + 1, last_good_line + 2] do
  if n > 0 && n <= length(lines) do
    IO.puts("Line #{n}: #{Enum.at(lines, n - 1)}")
  end
end
