# Analyze failing tests to categorize error types
defmodule FailureAnalyzer do
  @test_dir "purs_tests"

  def run() do
    test_files = Path.wildcard("#{@test_dir}/*.purs") |> Enum.sort()

    errors = test_files
      |> Enum.map(&analyze_test/1)
      |> Enum.filter(fn {_, result} -> match?({:error, _, _}, result) end)
      |> Enum.filter(fn {_, {:error, stage, _}} -> stage == :execute end)

    # Group by error type
    grouped = Enum.group_by(errors, fn {_, {:error, _, msg}} ->
      cond do
        String.contains?(to_string(msg), "undefined variable") -> :undefined_var
        String.contains?(to_string(msg), "undefined function") -> :undefined_func
        String.contains?(to_string(msg), "FunctionClauseError") -> :function_clause
        String.contains?(to_string(msg), "is already defined") -> :duplicate_def
        String.contains?(to_string(msg), "MatchError") -> :match_error
        String.contains?(to_string(msg), "ArithmeticError") -> :arithmetic_error
        true -> :other
      end
    end)

    IO.puts("\n=== Error Categories ===")
    Enum.each(grouped, fn {category, items} ->
      IO.puts("\n#{category}: #{length(items)} tests")
      items
      |> Enum.take(3)
      |> Enum.each(fn {file, {:error, _, msg}} ->
        name = Path.basename(file, ".purs")
        IO.puts("  #{name}: #{String.slice(to_string(msg), 0, 80)}")
      end)
    end)
  end

  defp analyze_test(file) do
    case File.read(file) do
      {:ok, source} ->
        case parse_and_compile(source) do
          {:ok, code} -> execute(code, file)
          error -> {file, error}
        end
      {:error, reason} ->
        {file, {:error, :read, reason}}
    end
  end

  defp parse_and_compile(source) do
    try do
      case Nova.Compiler.CstPipeline.parse_module_cst(source) do
        {:right, ast} ->
          code = Nova.Compiler.CodeGen.gen_module(ast)
          {:ok, code}
        {:left, error} ->
          {:error, :parse, error}
      end
    rescue
      e -> {:error, :compile, Exception.format(:error, e)}
    end
  end

  defp execute(code, file) do
    test_name = Path.basename(file, ".purs")
    module_name = "AnalyzeTest.#{test_name}" |> String.replace(~r/[^A-Za-z0-9_]/, "_")
    code = Regex.replace(~r/defmodule\s+\S+\s+do/, code, "defmodule #{module_name} do")

    try do
      Code.compile_string(code)
      {file, {:ok, :compiled}}
    rescue
      e -> {file, {:error, :execute, Exception.message(e)}}
    catch
      :exit, reason -> {file, {:error, :execute, inspect(reason)}}
    end
  end
end

FailureAnalyzer.run()
