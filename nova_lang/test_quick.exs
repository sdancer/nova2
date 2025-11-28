source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)

# Find around 3132 tokens
for n <- [3100, 3132, 3150, 3200, 3300, 3400, 3500] do
  subset = Enum.take(tokens, n)
  case Nova.Compiler.Parser.parse_module(subset) do
    {:right, {:tuple, mod, rest}} ->
      IO.puts("#{n} tokens: OK #{length(mod.declarations)} decls")
    {:left, err} ->
      IO.puts("#{n} tokens: FAIL")
  end
end

# Binary search between 3132 and 3500
IO.puts("\nBinary search 3132..3500:")
defmodule Search do
  def find(tokens, lo, hi, last_good) when lo >= hi, do: {last_good, hi}

  def find(tokens, lo, hi, last_good) do
    mid = div(lo + hi, 2)
    subset = Enum.take(tokens, mid)
    case Nova.Compiler.Parser.parse_module(subset) do
      {:right, _} -> find(tokens, mid + 1, hi, mid)
      {:left, _} -> find(tokens, lo, mid, last_good)
    end
  end
end

{last_good, fail_at} = Search.find(tokens, 3132, 3500, 3132)
IO.puts("Last good: #{last_good}, fails at: #{fail_at}")

# Show context
t = Enum.at(tokens, last_good)
IO.puts("Token #{last_good}: #{t.value} (#{t.token_type}) at line #{t.line}")
