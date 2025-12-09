# Test compiling all compiler modules using Nova.compile
# This tests the self-hosting capability of the Nova compiler

defmodule CompileAllTest do
  @base_path "../src/Nova/Compiler/"

  # Compiler modules in dependency order
  @compiler_files [
    "Ast.purs",
    "Types.purs",
    "Tokenizer.purs",
    "Unify.purs",
    "TypeChecker.purs",
    "CodeGen.purs",
    "Parser.purs",
    "Dependencies.purs"
  ]

  def run do
    IO.puts("=== Testing Nova Compiler Self-Compilation ===\n")

    results = Enum.map(@compiler_files, fn file ->
      path = Path.join(@base_path, file)
      name = String.replace(file, ".purs", "")
      test_compile(name, path)
    end)

    IO.puts("\n=== Summary ===")
    {success, failures} = Enum.split_with(results, fn {_, status, _} -> status == :ok end)

    Enum.each(results, fn {name, status, msg} ->
      icon = if status == :ok, do: "✓", else: "✗"
      IO.puts("  #{icon} #{name}: #{msg}")
    end)

    IO.puts("\nTotal: #{length(success)}/#{length(results)} succeeded")

    if length(failures) > 0 do
      System.halt(1)
    end
  end

  defp test_compile(name, path) do
    IO.puts("Testing #{name}...")

    case File.read(path) do
      {:error, reason} ->
        {name, :error, "File not found: #{reason}"}

      {:ok, source} ->
        # Test parse
        tokens = Nova.Compiler.Tokenizer.tokenize(source)

        case Nova.Compiler.Parser.parse_module(tokens) do
          {:left, err} ->
            {name, :error, "Parse error: #{inspect(err)}"}

          {:right, {:tuple, mod, _rest}} ->
            IO.puts("  Parsed: #{count_decls(mod.declarations)} declarations")

            # Test type check
            mod_decls = Nova.List.to_list(mod.declarations)
            env = Nova.Compiler.Types.empty_env()

            case Nova.Compiler.TypeChecker.check_module(env, mod_decls) do
              {:left, err} ->
                {name, :error, "Type error: #{format_error(err)}"}

              {:right, _env} ->
                IO.puts("  Type checked OK")

                # Test code gen
                try do
                  code = Nova.Compiler.CodeGen.gen_module().(mod)
                  lines = String.split(code, "\n") |> length()
                  IO.puts("  Generated: #{lines} lines")
                  {name, :ok, "#{lines} lines generated"}
                rescue
                  e ->
                    {name, :error, "CodeGen error: #{inspect(e)}"}
                end
            end
        end
    end
  end

  defp count_decls(list) do
    count_list(list, 0)
  end

  defp count_list(:nil, acc), do: acc
  defp count_list({:cons, _, rest}, acc), do: count_list(rest, acc + 1)
  defp count_list(_, acc), do: acc

  defp format_error(err) when is_binary(err), do: err
  defp format_error(err), do: inspect(err) |> String.slice(0, 100)
end

CompileAllTest.run()
