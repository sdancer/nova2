# Test parsing patterns
codes = [
  {"fmap with param", "test1 f xs = f <$> xs"},
  {"fmap point-free", "f = identity <$> identity"},
  {"fmap lambda point-free", "f = (\\x -> x) <$> (\\y -> y)"},
]

for {name, code} <- codes do
  IO.puts("\n=== #{name} ===")
  IO.puts("Code: #{code}")
  case Nova.compile("module M where\n\n#{code}") do
    {:ok, elixir} ->
      IO.puts("OK: compiled successfully")
    {:error, err} ->
      IO.puts("Error: #{inspect(err)}")
  end
end
