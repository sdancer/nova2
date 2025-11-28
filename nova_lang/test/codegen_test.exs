# CodeGen Tests - parse .purs files and generate Elixir
# Run with: mix run test/codegen_test.exs

defmodule CodeGenTest do
  def run do
    IO.puts("=== CodeGen Tests (Stage 2 Elixir) ===\n")

    test_files = [
      # Core compiler modules
      {"Ast.purs", "../src/Nova/Compiler/Ast.purs"},
      {"Types.purs", "../src/Nova/Compiler/Types.purs"},
      {"Unify.purs", "../src/Nova/Compiler/Unify.purs"},
      {"TypeChecker.purs", "../src/Nova/Compiler/TypeChecker.purs"},
      {"CodeGen.purs", "../src/Nova/Compiler/CodeGen.purs"},
      {"Parser.purs", "../src/Nova/Compiler/Parser.purs"},
      {"Dependencies.purs", "../src/Nova/Compiler/Dependencies.purs"},
      {"Tokenizer.purs", "../src/Nova/Compiler/Tokenizer.purs"}
    ]

    results = Enum.map(test_files, fn {name, path} ->
      test_codegen(name, path)
    end)

    passed = Enum.count(results, fn {status, _} -> status == :ok end)
    failed = length(results) - passed

    IO.puts("\n=== Results ===")
    IO.puts("Passed: #{passed}/#{length(results)}")
    if failed > 0, do: IO.puts("Failed: #{failed}")
    IO.puts("")

    if failed == 0 do
      IO.puts("✓ All codegen tests passed!")
    else
      IO.puts("✗ Some tests failed")
      System.halt(1)
    end
  end

  defp test_codegen(name, path) do
    case File.read(path) do
      {:ok, src} ->
        {time_us, result} = :timer.tc(fn ->
          try do
            tokens = Nova.FastTokenizer.tokenize(src)
            case Nova.Compiler.Parser.parse_module(tokens) do
              {:right, {:tuple, mod, remaining}} ->
                if length(remaining) > 0 do
                  {:error, "#{length(remaining)} unparsed tokens remaining"}
                else
                  code = Nova.Compiler.CodeGen.gen_module(mod)
                  # Verify generated code compiles
                  case Code.string_to_quoted(code) do
                    {:ok, _ast} -> {:ok, mod.name, byte_size(code)}
                    {:error, err} -> {:error, "Generated code doesn't parse: #{inspect(err)}"}
                  end
                end
              {:left, err} ->
                {:error, "Parse error: #{err}"}
            end
          rescue
            e -> {:error, "Exception: #{inspect(e)}"}
          end
        end)

        case result do
          {:ok, mod_name, code_size} ->
            IO.puts("✓ #{name} (#{mod_name}) - #{Float.round(time_us/1000, 1)}ms, #{code_size} bytes")
            {:ok, name}
          {:error, reason} ->
            IO.puts("✗ #{name}")
            IO.puts("  Error: #{reason}")
            {:error, name}
        end

      {:error, reason} ->
        IO.puts("✗ #{name}")
        IO.puts("  Cannot read file: #{inspect(reason)}")
        {:error, name}
    end
  end
end

CodeGenTest.run()
