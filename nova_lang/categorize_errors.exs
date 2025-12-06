# Categorize error patterns across failing tests
defmodule ErrorCategorizer do
  def run() do
    test_files = Path.wildcard("purs_tests/*.purs") |> Enum.sort()

    errors = test_files
      |> Enum.map(fn file -> {file, analyze_file(file)} end)
      |> Enum.filter(fn {_, result} -> match?({:error, :execute, _}, result) end)

    IO.puts("Execute errors: #{length(errors)}")

    # Categorize undefined variable/function errors
    undefined_items = errors
      |> Enum.flat_map(fn {_, {:error, :execute, msg}} ->
        # Extract undefined variable names
        var_matches = Regex.scan(~r/undefined variable "([^"]+)"/, msg)
        func_matches = Regex.scan(~r/undefined function ([a-zA-Z_]\w*)\//, msg)

        vars = Enum.map(var_matches, fn [_, v] -> v end)
        funcs = Enum.map(func_matches, fn [_, f] -> f end)

        vars ++ funcs
      end)
      |> Enum.frequencies()
      |> Enum.sort_by(fn {_, count} -> -count end)
      |> Enum.take(30)

    IO.puts("\n=== Most Common Undefined Variables/Functions ===")
    Enum.each(undefined_items, fn {var, count} ->
      IO.puts("  #{var}: #{count}")
    end)

    # Count other error types
    compile_errors = errors
      |> Enum.filter(fn {_, {:error, :execute, msg}} ->
        String.contains?(msg, "cannot compile module")
      end)
      |> length()

    already_defined = errors
      |> Enum.filter(fn {_, {:error, :execute, msg}} ->
        String.contains?(msg, "already been defined")
      end)
      |> length()

    expected_map = errors
      |> Enum.filter(fn {_, {:error, :execute, msg}} ->
        String.contains?(msg, "expected a map")
      end)
      |> length()

    IO.puts("\n=== Error Type Counts ===")
    IO.puts("  Compile errors (undefined vars/funcs): #{compile_errors}")
    IO.puts("  Already defined: #{already_defined}")
    IO.puts("  Expected map: #{expected_map}")
  end

  defp analyze_file(file) do
    case File.read(file) do
      {:ok, source} ->
        try do
          case Nova.Compiler.CstPipeline.parse_module_cst(source) do
            {:right, ast} ->
              try do
                elixir_code = Nova.Compiler.CodeGen.gen_module(ast)
                elixir_code = patch_imports(elixir_code)

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

  defp patch_imports(code) do
    code
    |> String.replace(Regex.compile!("\\(log\\)\\.\\(")  |> elem(1), "(&Nova.Runtime.log/1).(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)(?<!&)log\\.\\(") |> elem(1), "Nova.Runtime.log(")
    |> String.replace(Regex.compile!("\\(logShow\\)\\.\\(") |> elem(1), "(&Nova.Runtime.log_show/1).(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)log_show\\.\\(") |> elem(1), "Nova.Runtime.log_show(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)logShow\\.\\(") |> elem(1), "Nova.Runtime.log_show(")
    |> String.replace(Regex.compile!("\\(show\\)\\.\\(") |> elem(1), "(&Nova.Runtime.show/1).(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)(?<!&)show\\.\\(") |> elem(1), "Nova.Runtime.show(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)assert\\.\\(") |> elem(1), "Nova.Runtime.assert(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)assertEqual\\.\\(") |> elem(1), "Nova.Runtime.assert_equal(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)flip\\.\\(") |> elem(1), "Nova.Runtime.flip(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)const\\.\\(") |> elem(1), "Nova.Runtime.const_(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)identity\\.\\(") |> elem(1), "Nova.Runtime.identity(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)negate\\.\\(") |> elem(1), "Nova.Runtime.negate(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)coerce\\.\\(") |> elem(1), "Nova.Runtime.coerce(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)\\bunit\\b(?!\\.)") |> elem(1), "Nova.Runtime.unit()")
    |> String.replace(Regex.compile!("\\(pure\\)\\.\\(") |> elem(1), "(&Nova.Runtime.pure/1).(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)pure\\.\\(") |> elem(1), "Nova.Runtime.pure(")
    |> String.replace(Regex.compile!("\\(bind\\)\\.\\(") |> elem(1), "(&Nova.Runtime.bind/2).(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)bind\\.\\(") |> elem(1), "Nova.Runtime.bind(")
    |> String.replace(Regex.compile!("\\(map\\)\\.\\(") |> elem(1), "(&Nova.Runtime.map/2).(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)(?<!Enum\\.)map\\.\\(") |> elem(1), "Nova.Runtime.map(")
    |> String.replace(Regex.compile!("\\(apply\\)\\.\\(") |> elem(1), "(&Nova.Runtime.apply/2).(")
    |> String.replace(Regex.compile!("(?<!Runtime\\.)apply\\.\\(") |> elem(1), "Nova.Runtime.apply(")
  end
end

ErrorCategorizer.run()
