# Analyze execute failures
defmodule AnalyzeExecute do
  def run do
    # Get a few failing tests
    test_files = ["purs_tests/Console.purs", "purs_tests/CaseStatement.purs", "purs_tests/Church.purs"]

    Enum.each(test_files, fn file ->
      if File.exists?(file) do
        IO.puts("=== #{file} ===")
        analyze_file(file)
        IO.puts("")
      end
    end)
  end

  def analyze_file(file) do
    source = File.read!(file)

    case Nova.Compiler.CstPipeline.parse_module_cst(source) do
      {:right, ast} ->
        try do
          code = Nova.Compiler.CodeGen.gen_module().(ast)
          IO.puts("Generated code:")
          IO.puts(code)

          # Try to compile and execute
          try do
            Code.compile_string(code)
            IO.puts("✓ Compiled successfully")
          rescue
            e -> IO.puts("✗ Compile error: #{Exception.message(e)}")
          end
        rescue
          e -> IO.puts("✗ CodeGen error: #{Exception.message(e)}")
        end
      {:left, error} ->
        IO.puts("✗ Parse error: #{inspect(error)}")
    end
  end
end

AnalyzeExecute.run()
