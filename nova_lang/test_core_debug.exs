source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)
IO.puts("Tokens: #{length(tokens)}")

# Binary search for the failing point
defmodule Debug do
  def parse_subset(tokens, n) do
    subset = Enum.take(tokens, n)
    case Nova.Compiler.Parser.parse_module(subset) do
      {:right, result} -> {:ok, result}
      {:left, err} -> {:error, err}
      other ->
        IO.puts("Unknown result: #{inspect(other) |> String.slice(0, 100)}")
        {:error, "unknown"}
    end
  end

  def binary_search(tokens, lo, hi, last_good) when lo >= hi do
    {last_good, hi}
  end

  def binary_search(tokens, lo, hi, last_good) do
    mid = div(lo + hi, 2)
    case parse_subset(tokens, mid) do
      {:ok, _} ->
        # Try more
        binary_search(tokens, mid + 1, hi, mid)
      {:error, _} ->
        # Try less
        binary_search(tokens, lo, mid, last_good)
    end
  end
end

{last_good_n, fail_n} = Debug.binary_search(tokens, 1, length(tokens), 0)
IO.puts("Last good at token #{last_good_n}, fails at #{fail_n}")

if last_good_n > 0 do
  # Show context around failure point
  IO.puts("\nTokens around failure point:")
  for i <- max(0, last_good_n - 5)..(last_good_n + 5) do
    tok = Enum.at(tokens, i)
    if tok do
      marker = if i == last_good_n, do: ">>> ", else: "    "
      IO.puts("#{marker}#{i}: #{tok.value} (#{tok.token_type}) line #{tok.line} col #{tok.column}")
    end
  end

  # Parse to last_good_n and show what we got
  {:ok, result} = Debug.parse_subset(tokens, last_good_n)
  {mod, rest} = case result do
    {:tuple, m, r} -> {m, r}
    {m, r} -> {m, r}
    _ -> {nil, []}
  end

  if mod do
    IO.puts("\nParsed #{length(mod.declarations)} declarations")
    # Show last declaration
    case List.last(mod.declarations) do
      nil -> :ok
      last_decl -> IO.puts("Last decl: #{inspect(last_decl) |> String.slice(0, 200)}")
    end
  end

  # Show remaining tokens (first few)
  IO.puts("\nRemaining tokens (first 10):")
  rest |> Enum.take(10) |> Enum.each(fn tok ->
    IO.puts("  #{tok.value} (#{tok.token_type}) line #{tok.line} col #{tok.column}")
  end)
end
