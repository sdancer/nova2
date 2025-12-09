# Test type checking with the new dynamic type alias system
# Tests that type aliases are properly collected and used during unification

defmodule TypeCheckTest do
  def test_module(name, source_path) do
    IO.puts("\n=== Testing #{name} ===")
    source = File.read!(source_path)

    case Nova.Compiler.CstPipeline.parse_module_cst(source) do
      {:left, err} ->
        IO.puts("  Parse error: #{inspect(err)}")
        :parse_error
      {:right, ast} ->
        IO.puts("  Parsed: #{length(ast.declarations)} declarations")

        # Type check with empty env (no imports resolved)
        env = Nova.Compiler.Types.empty_env()
        case Nova.Compiler.TypeChecker.check_module(env, Nova.Array.from_foldable(ast.declarations)) do
          {:left, err} ->
            IO.puts("  Type check error: #{inspect(err)}")
            :type_error
          {:right, env2} ->
            # Check if type aliases were collected
            alias_count = map_size(env2.type_aliases)
            IO.puts("  Type check OK! Collected #{alias_count} type aliases")

            # Show which aliases were collected
            if alias_count > 0 do
              IO.puts("  Type aliases:")
              Enum.each(env2.type_aliases, fn {name, ty} ->
                ty_str = case ty do
                  {:ty_record, _} -> "record"
                  {:ty_con, tc} -> "#{tc.name}"
                  {:ty_var, v} -> "var(#{v.name})"
                  _ -> "other"
                end
                IO.puts("    - #{name}: #{ty_str}")
              end)
            end
            :ok
        end
    end
  end

  def run do
    modules = [
      {"Ast", "../src/Nova/Compiler/Ast.purs"},
      {"Types", "../src/Nova/Compiler/Types.purs"},
      {"Unify", "../src/Nova/Compiler/Unify.purs"},
    ]

    results = Enum.map(modules, fn {name, path} ->
      {name, test_module(name, path)}
    end)

    IO.puts("\n=== Summary ===")
    Enum.each(results, fn {name, result} ->
      status = case result do
        :ok -> "✓"
        :parse_error -> "✗ (parse)"
        :type_error -> "✗ (type)"
      end
      IO.puts("  #{status} #{name}")
    end)
  end
end

TypeCheckTest.run()
