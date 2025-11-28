# Benchmark Nova compiler phases

defmodule Benchmark do
  def run(source_path) do
    IO.puts("Benchmarking: #{source_path}")
    IO.puts(String.duplicate("-", 50))

    src = File.read!(source_path)
    IO.puts("Source: #{byte_size(src)} bytes, #{length(String.split(src, "\n"))} lines")

    # Time tokenization
    {tokenize_us, tokens} = :timer.tc(fn ->
      Nova.Compiler.Tokenizer.tokenize(src)
    end)
    IO.puts("Tokenize: #{Float.round(tokenize_us / 1000, 2)}ms (#{length(tokens)} tokens)")

    # Time parsing
    {parse_us, result} = :timer.tc(fn ->
      Nova.Compiler.Parser.parse_module(tokens)
    end)

    case result do
      {:right, {:tuple, mod, _}} ->
        IO.puts("Parse: #{Float.round(parse_us / 1000, 2)}ms (#{length(mod.declarations)} declarations)")

        # Time codegen
        {codegen_us, code} = :timer.tc(fn ->
          Nova.Compiler.CodeGen.gen_module(mod)
        end)
        lines = code |> String.split("\n") |> length()
        IO.puts("CodeGen: #{Float.round(codegen_us / 1000, 2)}ms (#{lines} lines output)")

        total = tokenize_us + parse_us + codegen_us
        IO.puts(String.duplicate("-", 50))
        IO.puts("Total compile time: #{Float.round(total / 1000, 2)}ms")

      {:left, err} ->
        IO.puts("Parse failed: #{inspect(err)}")
    end
  end
end

case System.argv() do
  [path] -> Benchmark.run(path)
  _ -> IO.puts("Usage: mix run benchmark.exs <source.purs>")
end
