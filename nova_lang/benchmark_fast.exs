# Benchmark Fast vs Slow tokenizer

defmodule BenchmarkFast do
  def run(source_path) do
    IO.puts("Benchmarking: #{source_path}")
    IO.puts(String.duplicate("-", 60))

    src = File.read!(source_path)
    IO.puts("Source: #{byte_size(src)} bytes, #{length(String.split(src, "\n"))} lines")
    IO.puts("")

    # Time slow tokenizer
    {slow_us, slow_tokens} = :timer.tc(fn ->
      Nova.Compiler.Tokenizer.tokenize(src)
    end)
    IO.puts("Slow tokenizer: #{Float.round(slow_us / 1000, 2)}ms (#{length(slow_tokens)} tokens)")

    # Time fast tokenizer
    {fast_us, fast_tokens} = :timer.tc(fn ->
      Nova.FastTokenizer.tokenize(src)
    end)
    IO.puts("Fast tokenizer: #{Float.round(fast_us / 1000, 2)}ms (#{length(fast_tokens)} tokens)")

    speedup = Float.round(slow_us / fast_us, 1)
    IO.puts("")
    IO.puts("Speedup: #{speedup}x faster")
  end
end

case System.argv() do
  [path] -> BenchmarkFast.run(path)
  _ -> IO.puts("Usage: mix run benchmark_fast.exs <source.purs>")
end
