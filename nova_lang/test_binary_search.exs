source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)
IO.puts("Total tokens: #{length(tokens)}")

defmodule BinarySearch do
  def find(tokens, lo, hi, last_good) when lo >= hi, do: last_good

  def find(tokens, lo, hi, last_good) do
    mid = div(lo + hi, 2)
    subset = Enum.take(tokens, mid)
    case Nova.Compiler.Parser.parse_module(subset) do
      {:right, _} -> find(tokens, mid + 1, hi, mid)
      {:left, _} -> find(tokens, lo, mid, last_good)
    end
  end
end

last_good = BinarySearch.find(tokens, 0, length(tokens), 0)
IO.puts("Last good token index: #{last_good}")

# Show context around failure
for i <- [last_good - 2, last_good - 1, last_good, last_good + 1, last_good + 2] do
  if i >= 0 && i < length(tokens) do
    t = Enum.at(tokens, i)
    IO.puts("Token #{i}: '#{t.value}' (#{t.token_type}) line #{t.line}")
  end
end

# Show how many decls we got at the last good point
last_good_tokens = Enum.take(tokens, last_good)
case Nova.Compiler.Parser.parse_module(last_good_tokens) do
  {:right, {:tuple, mod, _rest}} ->
    IO.puts("\nAt #{last_good} tokens: #{length(mod.declarations)} declarations")
  {:left, err} ->
    IO.puts("\nFailed at #{last_good}: #{inspect(err)}")
end
