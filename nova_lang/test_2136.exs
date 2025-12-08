source = File.read!("purs_tests/2136.purs")
IO.puts("=== Source ===")
IO.puts(source)
IO.puts("\n=== Compile ===")
case Nova.compile(source) do
  {:ok, code} ->
    IO.puts(code)
    IO.puts("\n=== Eval ===")
    try do
      Code.eval_string(code)
      IO.puts("Success!")
    rescue
      e -> IO.puts("Error: #{Exception.message(e)}")
    end
  {:error, err} ->
    IO.puts("Compile Error: #{inspect(err)}")
end
