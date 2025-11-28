source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")
code_109 = Enum.take(lines, 109) |> Enum.join("\n")

tokens = Nova.Compiler.Tokenizer.tokenize(code_109)
IO.puts("Tokens for 109 lines: #{length(tokens)}")

# Find tokens on line 109
tokens_line_109 = Enum.with_index(tokens) |> Enum.filter(fn {t, _} -> t.line == 109 end)
IO.puts("Tokens on line 109: #{length(tokens_line_109)}")
for {t, i} <- tokens_line_109 do
  IO.puts("  Token #{i}: '#{t.value}' (#{t.token_type})")
end

# Check what line 109 actually is
IO.puts("\nLine 109: #{Enum.at(lines, 108)}")

# Binary search by token count
defmodule BS do
  def find(toks, lo, hi, last) when lo >= hi, do: last
  def find(toks, lo, hi, last) do
    mid = div(lo + hi, 2)
    case Nova.Compiler.Parser.parse_module(Enum.take(toks, mid)) do
      {:right, _} -> find(toks, mid + 1, hi, mid)
      {:left, _} -> find(toks, lo, mid, last)
    end
  end
end

last_good = BS.find(tokens, 0, length(tokens), 0)
IO.puts("\nLast good token: #{last_good}")

# Show tokens around failure
for i <- [last_good-2, last_good-1, last_good, last_good+1, last_good+2] do
  if i >= 0 && i < length(tokens) do
    t = Enum.at(tokens, i)
    IO.puts("Token #{i}: '#{t.value}' (#{t.token_type}) line #{t.line}")
  end
end
