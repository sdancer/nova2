# Test tokenization of operators
codes = [
  "f = 1 + 2",
  "f = identity <$> identity",
  "f = x <*> y",
  "f = a <<< b",
  "f = a $ b",
]

for code <- codes do
  IO.puts("\n=== Code: #{code} ===")
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  IO.inspect(Enum.map(tokens, fn t -> {elem(t, 0), elem(t, 1)} end), label: "Tokens")
end
