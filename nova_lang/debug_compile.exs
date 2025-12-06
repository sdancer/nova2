# Debug a specific test file by showing the generated Elixir code

test_file = System.argv() |> List.first() || "purs_tests/1110.purs"

source = File.read!(test_file)

case Nova.Compiler.CstPipeline.parse_module_cst(source) do
  {:right, ast} ->
    code = Nova.Compiler.CodeGen.gen_module().(ast)
    IO.puts("=== Generated Elixir Code ===")
    IO.puts(code)
    IO.puts("\n=== Attempting to compile ===")
    try do
      Code.compile_string(code)
      IO.puts("OK - Compiled successfully")
    rescue
      e ->
        IO.puts("ERROR:")
        IO.puts(Exception.format(:error, e))
    end
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
