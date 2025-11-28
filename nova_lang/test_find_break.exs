source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Find where the break happens
# Lines 1-100 works with 45 decls
# Lines 1-150 fails

# Binary search between 100-150
defmodule Search do
  def find(lines, lo, hi, last_good, decl_count) when lo >= hi do
    {last_good, decl_count}
  end

  def find(lines, lo, hi, last_good, decl_count) do
    mid = div(lo + hi, 2)
    code = Enum.take(lines, mid) |> Enum.join("\n")
    tokens = Nova.Compiler.Tokenizer.tokenize(code)

    case Nova.Compiler.Parser.parse_module(tokens) do
      {:right, {:tuple, mod, rest}} when length(rest) == 0 ->
        # Success with no remaining tokens
        find(lines, mid + 1, hi, mid, length(mod.declarations))
      _ ->
        # Failure or leftover tokens
        find(lines, lo, mid, last_good, decl_count)
    end
  end
end

{last_good, decls} = Search.find(lines, 100, 150, 100, 45)
IO.puts("Last good line: #{last_good} (#{decls} decls)")

# Now narrow down
IO.puts("\nNarrowing around #{last_good}:")
for n <- [last_good - 2, last_good - 1, last_good, last_good + 1, last_good + 2, last_good + 3] do
  code = Enum.take(lines, n) |> Enum.join("\n")
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, _} -> "FAIL"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("  Lines 1-#{n}: #{result}")
end

# Show what's on the last good line and next few
IO.puts("\nContent around failure:")
for n <- [last_good - 1, last_good, last_good + 1, last_good + 2] do
  IO.puts("  Line #{n}: #{Enum.at(lines, n - 1)}")
end
