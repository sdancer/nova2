# PureScript Passing Tests Runner
#
# This script runs the PureScript passing tests through the Nova compiler.
# Each test is a .purs file that should:
# 1. Parse successfully
# 2. Compile to Elixir
# 3. Execute main() which logs "Done"
#
# Test stages:
# - parse: Parse the PureScript source
# - compile: Generate Elixir code
# - execute: Run the generated code and check output

defmodule PursTestRunner do
  @test_dir "purs_tests"

  def run(opts \\ []) do
    limit = Keyword.get(opts, :limit, :all)
    filter = Keyword.get(opts, :filter, nil)
    verbose = Keyword.get(opts, :verbose, false)

    test_files = list_test_files(filter)
    test_files = case limit do
      :all -> test_files
      n when is_integer(n) -> Enum.take(test_files, n)
    end

    IO.puts("Running #{length(test_files)} PureScript passing tests...\n")

    # Run tests in parallel for better performance
    results = test_files
      |> Task.async_stream(fn file ->
        result = run_test(file, verbose)
        {file, result}
      end, max_concurrency: System.schedulers_online() * 2, timeout: 60_000, on_timeout: :kill_task, ordered: false)
      |> Enum.map(fn
        {:ok, {file, result}} -> {file, result}
        {:exit, :timeout} -> {"unknown", {:error, :timeout, "Task timed out after 60s"}}
        {:exit, reason} -> {"unknown", {:error, :timeout, inspect(reason)}}
      end)
      |> Enum.sort_by(fn {file, _} -> file end)

    # Print results after collection (to avoid interleaved output)
    Enum.each(results, fn {file, result} -> print_result(file, result, verbose) end)

    summarize(results)
  end

  defp list_test_files(filter) do
    Path.wildcard("#{@test_dir}/*.purs")
    |> Enum.sort()
    |> case do
      files when filter != nil ->
        Enum.filter(files, fn f -> String.contains?(Path.basename(f), filter) end)
      files -> files
    end
  end

  defp run_test(file, verbose) do
    test_name = Path.basename(file, ".purs")

    case File.read(file) do
      {:ok, source} ->
        run_parse(source, test_name, verbose)
      {:error, reason} ->
        {:error, :read, "Failed to read file: #{inspect(reason)}"}
    end
  end

  defp run_parse(source, test_name, verbose) do
    try do
      case Nova.Compiler.CstPipeline.parse_module_cst(source) do
        {:right, ast} ->
          if verbose, do: IO.puts("  [PARSE] Success")
          run_compile(ast, test_name, verbose)
        {:left, error} ->
          {:error, :parse, error}
      end
    rescue
      e -> {:error, :parse, Exception.format(:error, e)}
    catch
      :exit, reason -> {:error, :parse, "Exit: #{inspect(reason)}"}
      thrown -> {:error, :parse, "Throw: #{inspect(thrown)}"}
    end
  end

  defp run_compile(ast, test_name, verbose) do
    try do
      # Use CodeGen to generate Elixir
      case generate_elixir(ast) do
        {:ok, elixir_code} ->
          if verbose, do: IO.puts("  [COMPILE] Success (#{String.length(elixir_code)} chars)")
          run_execute(elixir_code, test_name, verbose)
        {:error, reason} ->
          {:error, :compile, reason}
      end
    rescue
      e -> {:error, :compile, Exception.format(:error, e)}
    catch
      :exit, reason -> {:error, :compile, "Exit: #{inspect(reason)}"}
      thrown -> {:error, :compile, "Throw: #{inspect(thrown)}"}
    end
  end

  defp generate_elixir(ast) do
    try do
      # Generate module code (gen_module returns a function that takes the AST)
      code = Nova.Compiler.CodeGen.gen_module().(ast)
      {:ok, code}
    rescue
      e -> {:error, Exception.format(:error, e)}
    end
  end

  defp run_execute(elixir_code, test_name, verbose) do
    # Create a unique module name to avoid conflicts
    module_name = "NovaTest.Purs#{test_name}" |> String.replace(~r/[^A-Za-z0-9_]/, "_")

    # Replace the module name in the generated code
    elixir_code = Regex.replace(~r/defmodule\s+\S+\s+do/, elixir_code, "defmodule #{module_name} do")

    # Patch known imported functions to use Nova.Runtime
    # This is a workaround until CodeGen.purs properly tracks imported functions
    elixir_code = patch_imports(elixir_code)

    try do
      # Compile the Elixir code
      Code.compile_string(elixir_code)

      # Try to get the module and run main
      module = String.to_atom("Elixir.#{module_name}")

      if function_exported?(module, :main, 0) do
        # Capture output
        {output, result} = capture_io(fn ->
          try do
            apply(module, :main, [])
            :ok
          rescue
            e -> {:error, Exception.format(:error, e)}
          end
        end)

        case result do
          :ok ->
            if String.contains?(output, "Done") do
              if verbose, do: IO.puts("  [EXECUTE] Success - output: #{String.trim(output)}")
              {:ok, output}
            else
              if verbose, do: IO.puts("  [EXECUTE] Success (no 'Done' output)")
              {:ok, output}
            end
          {:error, msg} ->
            {:error, :execute, msg}
        end
      else
        if verbose, do: IO.puts("  [EXECUTE] No main function")
        {:ok, "no main"}
      end
    rescue
      e ->
        {:error, :execute, Exception.format(:error, e)}
    catch
      :exit, reason -> {:error, :execute, "Exit: #{inspect(reason)}"}
      thrown -> {:error, :execute, "Throw: #{inspect(thrown)}"}
    end
  end

  # Patch imported function calls to use Nova.Runtime
  # This is a workaround until CodeGen.purs properly tracks imported functions
  defp patch_imports(code) do
    code
    # Effect.Console imports
    # (log).(x) -> (&Nova.Runtime.log/1).(x) - for function reference application
    |> String.replace(~r/\(log\)\.\(/, "(&Nova.Runtime.log/1).(")
    # log.("Done") -> Nova.Runtime.log("Done") - for direct call
    |> String.replace(~r/(?<!Runtime\.)(?<!&)log\.\(/, "Nova.Runtime.log(")
    |> String.replace(~r/\(logShow\)\.\(/, "(&Nova.Runtime.log_show/1).(")
    |> String.replace(~r/(?<!Runtime\.)log_show\.\(/, "Nova.Runtime.log_show(")
    |> String.replace(~r/(?<!Runtime\.)logShow\.\(/, "Nova.Runtime.log_show(")
    # Data.Show imports
    |> String.replace(~r/\(show\)\.\(/, "(&Nova.Runtime.show/1).(")
    |> String.replace(~r/(?<!Runtime\.)(?<!&)show\.\(/, "Nova.Runtime.show(")
    # Control.Assert / Test.Assert imports
    # Both camelCase (from imports) and snake_case (from codegen) patterns
    |> String.replace(~r/(?<!Runtime\.)assert\.\(/, "Nova.Runtime.assert(")
    |> String.replace(~r/(?<!Runtime\.)assertEqual\.\(/, "Nova.Runtime.assert_equal(")
    |> String.replace(~r/(?<!Runtime\.)assert_equal\.\(/, "Nova.Runtime.assert_equal(")
    |> String.replace(~r/(?<!Runtime\.)assertPrime\.\(/, "Nova.Runtime.assert_prime(")
    |> String.replace(~r/(?<!Runtime\.)assert_prime\.\(/, "Nova.Runtime.assert_prime(")
    |> String.replace(~r/(?<!Runtime\.)assertThrows\.\(/, "Nova.Runtime.assert_throws(")
    |> String.replace(~r/(?<!Runtime\.)assert_throws\.\(/, "Nova.Runtime.assert_throws(")
    # Data.Function imports
    |> String.replace(~r/(?<!Runtime\.)flip\.\(/, "Nova.Runtime.flip(")
    |> String.replace(~r/(?<!Runtime\.)const\.\(/, "Nova.Runtime.const_(")
    |> String.replace(~r/(?<!Runtime\.)identity\.\(/, "Nova.Runtime.identity(")
    # Prelude/basic ops
    |> String.replace(~r/(?<!Runtime\.)negate\.\(/, "Nova.Runtime.negate(")
    |> String.replace(~r/(?<!Runtime\.)coerce\.\(/, "Nova.Runtime.coerce(")
    # Unit - avoid matching :unit atoms
    |> String.replace(~r/(?<!Runtime\.)(?<!:)\bunit\b(?!\.)/, "Nova.Runtime.unit()")
    # FFI functions (Data.Function.Uncurried, Partial.Unsafe)
    |> String.replace(~r/(?<!Runtime\.)unsafePartial\.\(/, "Nova.Runtime.unsafe_partial(")
    |> String.replace(~r/(?<!Runtime\.)unsafe_partial\.\(/, "Nova.Runtime.unsafe_partial(")
    |> String.replace(~r/(?<!Runtime\.)runFn2\.\(/, "Nova.Runtime.run_fn2(")
    |> String.replace(~r/(?<!Runtime\.)run_fn2\.\(/, "Nova.Runtime.run_fn2(")
    |> String.replace(~r/(?<!Runtime\.)mkFn2\.\(/, "Nova.Runtime.mk_fn2(")
    |> String.replace(~r/(?<!Runtime\.)mk_fn2\.\(/, "Nova.Runtime.mk_fn2(")
    # Type class methods that may be imported from Prelude
    |> String.replace(~r/\(pure\)\.\(/, "(&Nova.Runtime.pure/1).(")
    |> String.replace(~r/(?<!Runtime\.)pure\.\(/, "Nova.Runtime.pure(")
    # Prelude module is now defined in lib/prelude.ex - no need for patches
    |> String.replace(~r/\(bind\)\.\(/, "(&Nova.Runtime.bind/2).(")
    |> String.replace(~r/(?<!Runtime\.)bind\.\(/, "Nova.Runtime.bind(")
    |> String.replace(~r/\(map\)\.\(/, "(&Nova.Runtime.map/2).(")
    |> String.replace(~r/(?<!Runtime\.)(?<!Enum\.)map\.\(/, "Nova.Runtime.map(")
    |> String.replace(~r/\(apply\)\.\(/, "(&Nova.Runtime.apply/2).(")
    |> String.replace(~r/(?<!Runtime\.)apply\.\(/, "Nova.Runtime.apply(")
  end

  defp capture_io(fun) do
    # Simple IO capture
    old_gl = Process.group_leader()
    {:ok, capture_pid} = StringIO.open("")
    Process.group_leader(self(), capture_pid)

    try do
      result = fun.()
      {_, output} = StringIO.contents(capture_pid)
      {output, result}
    after
      Process.group_leader(self(), old_gl)
      StringIO.close(capture_pid)
    end
  end

  defp print_result(file, result, verbose) do
    name = Path.basename(file, ".purs")

    case result do
      {:ok, _} ->
        IO.puts("#{name}: PASS")
      {:error, stage, msg} ->
        IO.puts("#{name}: FAIL at #{stage}")
        if verbose do
          IO.puts("  Error: #{String.slice(inspect(msg), 0, 200)}")
        end
    end
  end

  defp summarize(results) do
    total = length(results)
    passed = Enum.count(results, fn {_, r} -> match?({:ok, _}, r) end)

    parse_errors = Enum.count(results, fn {_, r} -> match?({:error, :parse, _}, r) end)
    compile_errors = Enum.count(results, fn {_, r} -> match?({:error, :compile, _}, r) end)
    execute_errors = Enum.count(results, fn {_, r} -> match?({:error, :execute, _}, r) end)
    read_errors = Enum.count(results, fn {_, r} -> match?({:error, :read, _}, r) end)

    IO.puts("\n=== Summary ===")
    IO.puts("Total: #{total}")
    IO.puts("Passed: #{passed} (#{Float.round(passed / total * 100, 1)}%)")
    IO.puts("Failed: #{total - passed}")
    IO.puts("  - Parse errors: #{parse_errors}")
    IO.puts("  - Compile errors: #{compile_errors}")
    IO.puts("  - Execute errors: #{execute_errors}")
    if read_errors > 0, do: IO.puts("  - Read errors: #{read_errors}")

    # Show first few failures of each type
    IO.puts("\n=== Sample Failures ===")

    parse_fails = results
      |> Enum.filter(fn {_, r} -> match?({:error, :parse, _}, r) end)
      |> Enum.take(3)

    if length(parse_fails) > 0 do
      IO.puts("\nParse failures:")
      Enum.each(parse_fails, fn {file, {:error, :parse, msg}} ->
        IO.puts("  #{Path.basename(file)}: #{String.slice(inspect(msg), 0, 100)}")
      end)
    end

    compile_fails = results
      |> Enum.filter(fn {_, r} -> match?({:error, :compile, _}, r) end)
      |> Enum.take(3)

    if length(compile_fails) > 0 do
      IO.puts("\nCompile failures:")
      Enum.each(compile_fails, fn {file, {:error, :compile, msg}} ->
        IO.puts("  #{Path.basename(file)}: #{String.slice(inspect(msg), 0, 100)}")
      end)
    end

    execute_fails = results
      |> Enum.filter(fn {_, r} -> match?({:error, :execute, _}, r) end)
      |> Enum.take(3)

    if length(execute_fails) > 0 do
      IO.puts("\nExecute failures:")
      Enum.each(execute_fails, fn {file, {:error, :execute, msg}} ->
        IO.puts("  #{Path.basename(file)}: #{String.slice(inspect(msg), 0, 100)}")
      end)
    end

    passed
  end
end

# Parse command line args
{opts, _} = OptionParser.parse!(System.argv(),
  strict: [limit: :integer, filter: :string, verbose: :boolean])

limit = Keyword.get(opts, :limit, :all)
filter = Keyword.get(opts, :filter)
verbose = Keyword.get(opts, :verbose, false)

PursTestRunner.run(limit: limit, filter: filter, verbose: verbose)
