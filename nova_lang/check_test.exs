test_name = System.argv() |> List.first() || "CaseInDo"
test_path = "purs_tests/#{test_name}.purs"

IO.puts("=== Test: #{test_name} ===")
IO.puts("File: #{test_path}")

source = File.read!(test_path)
IO.puts("\n--- Source ---")
IO.puts(source)
IO.puts("\n--- Compile Result ---")

case Nova.compile(source) do
  {:ok, code} ->
    IO.puts(code)
    IO.puts("\n--- Execute Result ---")
    try do
      {result, _} = Code.eval_string(code)
      IO.puts("Success: #{inspect(result)}")
    rescue
      e ->
        IO.puts("Runtime Error: #{inspect(e)}")
    catch
      :error, e ->
        IO.puts("Error: #{inspect(e)}")
    end
  {:error, err} ->
    IO.puts("Compile Error: #{inspect(err)}")
end
