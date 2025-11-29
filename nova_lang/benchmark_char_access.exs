# Benchmark 3 approaches for O(1) character access

source = String.duplicate("abcdefghij", 5000)  # 50,000 chars
IO.puts("String length: #{String.length(source)} chars\n")

# Positions to test
positions = [100, 1000, 5000, 10000, 25000, 49999]
iterations = 10000

IO.puts("=== Approach 1: String.at (current - O(n)) ===")
for pos <- positions do
  {us, _} = :timer.tc(fn ->
    for _ <- 1..iterations, do: String.at(source, pos)
  end)
  IO.puts("  pos #{pos}: #{Float.round(us/1000, 1)}ms")
end

IO.puts("\n=== Approach 2: Charlist + Enum.at ===")
charlist = String.to_charlist(source)
IO.puts("  (conversion: #{elem(:timer.tc(fn -> String.to_charlist(source) end), 0) / 1000}ms)")
for pos <- positions do
  {us, _} = :timer.tc(fn ->
    for _ <- 1..iterations, do: Enum.at(charlist, pos)
  end)
  IO.puts("  pos #{pos}: #{Float.round(us/1000, 1)}ms")
end

IO.puts("\n=== Approach 3: :binary.at (O(1) for bytes) ===")
for pos <- positions do
  {us, _} = :timer.tc(fn ->
    for _ <- 1..iterations, do: :binary.at(source, pos)
  end)
  IO.puts("  pos #{pos}: #{Float.round(us/1000, 1)}ms")
end

IO.puts("\n=== Approach 4: Tuple from charlist (true O(1)) ===")
tuple = source |> String.to_charlist() |> List.to_tuple()
IO.puts("  (conversion: #{elem(:timer.tc(fn -> source |> String.to_charlist() |> List.to_tuple() end), 0) / 1000}ms)")
for pos <- positions do
  {us, _} = :timer.tc(fn ->
    for _ <- 1..iterations, do: elem(tuple, pos)
  end)
  IO.puts("  pos #{pos}: #{Float.round(us/1000, 1)}ms")
end

IO.puts("\n=== Approach 5: Array (ETS-like, true O(1)) ===")
arr = :array.from_list(String.to_charlist(source))
IO.puts("  (conversion: #{elem(:timer.tc(fn -> :array.from_list(String.to_charlist(source)) end), 0) / 1000}ms)")
for pos <- positions do
  {us, _} = :timer.tc(fn ->
    for _ <- 1..iterations, do: :array.get(pos, arr)
  end)
  IO.puts("  pos #{pos}: #{Float.round(us/1000, 1)}ms")
end

IO.puts("\n=== Summary ===")
IO.puts("For tokenizer, best approach is :binary.at or tuple+elem")
IO.puts("These give O(1) access without position-dependent slowdown")
