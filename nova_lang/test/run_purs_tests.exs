# Run .purs test files
# Usage: mix run test/run_purs_tests.exs

defmodule PursTestRunner do
  def run do
    IO.puts("╔══════════════════════════════════════════════════════════════╗")
    IO.puts("║          Nova PureScript Test Runner (Stage 2)              ║")
    IO.puts("╚══════════════════════════════════════════════════════════════╝\n")

    test_files = Path.wildcard("test/purs/**/*.purs")

    if test_files == [] do
      IO.puts("No .purs test files found in test/purs/")
      System.halt(1)
    end

    results = Enum.map(test_files, &run_test_file/1)

    passed = Enum.count(results, fn {status, _, _} -> status == :ok end)
    failed = length(results) - passed

    IO.puts("\n══════════════════════════════════════════════════════════════")
    IO.puts("Results: #{passed} passed, #{failed} failed out of #{length(results)}")

    if failed > 0 do
      IO.puts("\nFailed:")
      Enum.each(results, fn
        {:error, path, reason} -> IO.puts("  ✗ #{path}: #{reason}")
        _ -> :ok
      end)
      System.halt(1)
    else
      IO.puts("\n✓ All .purs tests passed!")
    end
  end

  defp run_test_file(path) do
    IO.puts("Testing: #{path}")

    with {:ok, src} <- File.read(path),
         {:ok, code, mod_name} <- compile_purs(src),
         {:ok, module} <- load_module(code, path),
         {:ok, test_result} <- run_tests(module, mod_name) do
      IO.puts("  ✓ #{mod_name}: #{test_result}")
      {:ok, path, test_result}
    else
      {:error, reason} ->
        IO.puts("  ✗ #{reason}")
        {:error, path, reason}
    end
  end

  defp compile_purs(src) do
    try do
      tokens = Nova.FastTokenizer.tokenize(src)

      case Nova.Compiler.Parser.parse_module(tokens) do
        {:right, {:tuple, mod, remaining}} ->
          if length(remaining) > 0 do
            {:error, "#{length(remaining)} unparsed tokens"}
          else
            code = Nova.Compiler.CodeGen.gen_module(mod)
            {:ok, code, mod.name}
          end

        {:left, err} ->
          {:error, "Parse error: #{err}"}
      end
    rescue
      e -> {:error, "Compile error: #{Exception.message(e)}"}
    end
  end

  defp load_module(code, path) do
    try do
      # Verify the code parses
      case Code.string_to_quoted(code) do
        {:ok, _} ->
          # Compile the module
          [{module, _}] = Code.compile_string(code, path)
          {:ok, module}
        {:error, {line, msg, _}} ->
          {:error, "Elixir syntax error at line #{line}: #{msg}"}
      end
    rescue
      e -> {:error, "Load error: #{Exception.message(e)}"}
    end
  end

  defp run_tests(module, _mod_name) do
    try do
      cond do
        # Run runTests if it exists
        function_exported?(module, :run_tests, 0) ->
          result = apply(module, :run_tests, [])
          {:ok, "run_tests = #{inspect(result)}"}

        # Or try main
        function_exported?(module, :main, 0) ->
          result = apply(module, :main, [])
          {:ok, "main returned"}

        true ->
          # Just verify it compiled - list exported functions
          exports = module.__info__(:functions)
          func_names = exports |> Enum.map(fn {name, arity} -> "#{name}/#{arity}" end) |> Enum.join(", ")
          {:ok, "compiled (#{length(exports)} functions: #{func_names})"}
      end
    rescue
      e -> {:error, "Runtime error: #{Exception.message(e)}"}
    end
  end
end

PursTestRunner.run()
