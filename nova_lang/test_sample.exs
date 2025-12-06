code = File.read!("purs_tests/1185.purs")
IO.puts("=== Source ===")
IO.puts(code)

IO.puts("\n=== Compiling ===")
case Nova.compile(code) do
  {:ok, elixir} ->
    IO.puts(elixir)
    IO.puts("\n=== Loading ===")
    try do
      Code.compile_string(elixir)
      IO.puts("SUCCESS!")
    rescue
      e -> IO.puts("LOAD ERROR: #{Exception.message(e)}")
    end
  {:error, err} -> IO.puts("COMPILE ERROR: #{inspect(err)}")
end
