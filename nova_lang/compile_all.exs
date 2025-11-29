# Multi-file compiler with cross-module import resolution
# Usage: mix run compile_all.exs [--check]

defmodule MultiFileCompiler do
  @source_dir "../src/Nova/Compiler"

  @modules [
    "Ast",
    "Types",
    "Tokenizer",
    "Unify",
    "TypeChecker",
    "CodeGen",
    "Parser",
    "Dependencies"
  ]

  def main(args) do
    check_types = "--check" in args

    IO.puts("=== Phase 1: Parsing all modules ===")
    parsed = parse_all_modules()

    if check_types do
      IO.puts("\n=== Phase 2: Building registry ===")
      registry = build_registry(parsed)
      IO.puts("Registry contains #{map_size(registry)} modules")

      IO.puts("\n=== Phase 3: Type checking with imports ===")
      type_check_all(parsed, registry)
    else
      IO.puts("\nSkipping type check (use --check to enable)")
    end

    IO.puts("\n=== Done ===")
  end

  def parse_all_modules do
    Enum.reduce(@modules, %{}, fn mod_name, acc ->
      path = Path.join(@source_dir, "#{mod_name}.purs")
      IO.write("  Parsing #{mod_name}... ")

      src = File.read!(path)
      tokens = Nova.Compiler.Tokenizer.tokenize(src)

      case Nova.Compiler.Parser.parse_module(tokens) do
        {:right, {:tuple, mod, _rest}} ->
          IO.puts("OK (#{length(mod.declarations)} declarations)")
          Map.put(acc, mod.name, mod)
        {:left, err} ->
          IO.puts("FAILED: #{inspect(err)}")
          acc
      end
    end)
  end

  def build_registry(parsed) do
    Enum.reduce(parsed, %{}, fn {mod_name, mod}, registry ->
      IO.write("  Extracting exports from #{mod_name}... ")
      exports = Nova.Compiler.TypeChecker.extract_exports(mod.declarations)
      ctor_count = map_size(exports.constructors)
      type_count = map_size(exports.types)
      alias_count = map_size(exports.type_aliases)
      val_count = map_size(exports.values)
      IO.puts("#{ctor_count} ctors, #{type_count} types, #{alias_count} aliases, #{val_count} values")
      Nova.Compiler.Types.register_module(registry, mod_name, exports)
    end)
  end

  def type_check_all(parsed, registry) do
    results = Enum.map(@modules, fn mod_name ->
      full_name = "Nova.Compiler.#{mod_name}"
      case Map.get(parsed, full_name) do
        nil ->
          IO.puts("  #{mod_name}: SKIPPED (not parsed)")
          {:skip, mod_name}
        mod ->
          IO.write("  #{mod_name}: ")
          env = Nova.Compiler.Types.empty_env()
          case Nova.Compiler.TypeChecker.check_module_with_registry(registry, env, mod.declarations) do
            {:right, _} ->
              IO.puts("OK")
              {:ok, mod_name}
            {:left, err} ->
              IO.puts("FAILED - #{format_error(err)}")
              {:error, mod_name, err}
          end
      end
    end)

    ok_count = Enum.count(results, fn
      {:ok, _} -> true
      _ -> false
    end)
    fail_count = Enum.count(results, fn
      {:error, _, _} -> true
      _ -> false
    end)
    IO.puts("\nResults: #{ok_count} OK, #{fail_count} failed")
  end

  defp format_error({:unbound_variable, name}), do: "unbound variable: #{name}"
  defp format_error({:unify_err, err}), do: "unify error: #{inspect(err)}"
  defp format_error(err), do: inspect(err)
end

MultiFileCompiler.main(System.argv())
