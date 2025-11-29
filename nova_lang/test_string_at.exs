# Prove String.at is O(n)
source = String.duplicate("x", 50000)

IO.puts("Testing String.at performance (should be O(n)):\n")

for pos <- [100, 1000, 5000, 10000, 25000, 50000] do
  {us, _} = :timer.tc(fn ->
    for _ <- 1..1000, do: String.at(source, pos - 1)
  end)
  IO.puts("  pos #{pos}: #{Float.round(us/1000, 1)}ms for 1000 calls")
end

IO.puts("\nConclusion: String.at(s, n) takes O(n) time because")
IO.puts("UTF-8 strings must be scanned from the beginning.")
IO.puts("\nFix: Convert string to charlist or use binary matching.")
