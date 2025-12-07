# Debug script to generate and inspect code for specific tests

test_name = System.argv() |> List.first() || "Where"
test_path = "purs_tests/#{test_name}.purs"

case File.read(test_path) do
  {:ok, source} ->
    IO.puts("=== Source (#{test_path}) ===\n#{source}")

    case Nova.Compiler.CstPipeline.parse_module_cst(source) do
      {:right, ast} ->
        IO.puts("\n=== AST parsed successfully ===")
        code = Nova.Compiler.CodeGen.gen_module().(ast)
        IO.puts("\n=== Generated Elixir Code ===\n#{code}")

        # Try to compile
        try do
          Code.compile_string(code)
          IO.puts("\n=== Compilation successful ===")
        rescue
          e ->
            IO.puts("\n=== Compilation failed ===")
            IO.puts(Exception.format(:error, e))
        end

      {:left, err} ->
        IO.puts("Parse error: #{inspect(err)}")
    end

  {:error, reason} ->
    IO.puts("Could not read file: #{inspect(reason)}")
end
