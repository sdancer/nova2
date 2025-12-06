# Analyze error patterns across failing tests
defmodule ErrorAnalyzer do
  def run() do
    test_files = Path.wildcard("purs_tests/*.purs") |> Enum.sort()

    # Sample some failing tests to see error patterns
    errors = test_files
      |> Enum.map(fn file ->
        {file, analyze_file(file)}
      end)
      |> Enum.filter(fn {_, result} -> match?({:error, _, _}, result) end)
      |> Enum.take(20)

    IO.puts("=== Error Analysis ===\n")

    # Group by error type
    execute_errors = Enum.filter(errors, fn {_, {:error, :execute, _}} -> true; _ -> false end)

    IO.puts("Execute errors (#{length(execute_errors)}):")
    Enum.each(execute_errors, fn {file, {:error, :execute, msg}} ->
      basename = Path.basename(file, ".purs")
      # Extract key error patterns
      cond do
        String.contains?(msg, "already been defined") ->
          IO.puts("  #{basename}: DUPLICATE FUNCTION")
        String.contains?(msg, "undefined variable") ->
          [_, var] = Regex.run(~r/undefined variable "([^"]+)"/, msg) || [nil, "?"]
          IO.puts("  #{basename}: UNDEFINED VAR: #{var}")
        String.contains?(msg, "undefined function") ->
          [_, func] = Regex.run(~r/undefined function ([^\s]+)/, msg) || [nil, "?"]
          IO.puts("  #{basename}: UNDEFINED FUNC: #{func}")
        String.contains?(msg, "does not implement") ->
          IO.puts("  #{basename}: NOT IMPLEMENTED")
        String.contains?(msg, "expected a map") ->
          IO.puts("  #{basename}: EXPECTED MAP")
        true ->
          IO.puts("  #{basename}: #{String.slice(msg, 0, 80)}")
      end
    end)
  end

  defp analyze_file(file) do
    case File.read(file) do
      {:ok, source} ->
        try do
          case Nova.Compiler.CstPipeline.parse_module_cst(source) do
            {:right, ast} ->
              try do
                elixir_code = Nova.Compiler.CodeGen.gen_module(ast)

                try do
                  Code.compile_string(elixir_code)
                  {:ok, :compiled}
                rescue
                  e -> {:error, :execute, Exception.message(e)}
                end
              rescue
                e -> {:error, :compile, Exception.message(e)}
              end
            {:left, error} ->
              {:error, :parse, error}
          end
        rescue
          e -> {:error, :parse, Exception.message(e)}
        end
      {:error, reason} ->
        {:error, :read, reason}
    end
  end
end

ErrorAnalyzer.run()
