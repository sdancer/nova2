# Nova IEx startup - loads all Core Erlang modules
defmodule Nova.Loader do
  def load_all do
    core_dir = "priv/core"

    # Order matters - dependencies first
    modules = [
      "Control/Lazy.core",
      "Data/Maybe.core",
      "Data/Tuple.core",
      "Data/Either.core",
      "Data/Char.core",
      "Data/Int.core",
      "Data/Number.core",
      "Data/Array.core",
      "Data/List.core",
      "Data/String.core",
      "Data/String/CodeUnits.core",
      "Data/Foldable.core",
      "Data/Map.core",
      "Data/Set.core",
      "Data/Json.core",
      "Nova/Prelude.core",
      "OTP/Ets.core",
      "OTP/PersistentTerm.core",
      "Nova/NamespaceService.core",
      "Nova/Eval.core",
      "Nova/Web.core",
      "Nova/Compiler/Ast.core",
      "Nova/Compiler/Types.core",
      "Nova/Compiler/Cst.core",
      "Nova/Compiler/CstLayout.core",
      "Nova/Compiler/CstLexer.core",
      "Nova/Compiler/CstParser.core",
      "Nova/Compiler/CstToAst.core",
      "Nova/Compiler/CstPipeline.core",
      "Nova/Compiler/Unify.core",
      "Nova/Compiler/ImportProcessor.core",
      "Nova/Compiler/TypeChecker.core",
      "Nova/Compiler/RefEq.core",
      "Nova/Compiler/Dependencies.core",
      "Nova/Compiler/CodeGenCoreErlang.core",
      "Nova/Compiler/Regenerate.core",
    ]

    IO.puts("\n=== Loading Nova Core Erlang modules ===\n")

    results = Enum.map(modules, fn rel_path ->
      path = Path.join(core_dir, rel_path)
      mod_name = rel_path |> String.replace("/", ".") |> String.replace(".core", "")

      case File.read(path) do
        {:ok, content} ->
          case Nova.CoreErlangEval.compile_and_load(content) do
            {:ok, _mod} ->
              IO.puts("  ✓ #{mod_name}")
              :ok
            {:error, reason} ->
              IO.puts("  ✗ #{mod_name}: #{inspect(reason)}")
              :error
          end
        {:error, reason} ->
          IO.puts("  ✗ #{mod_name}: file error #{inspect(reason)}")
          :error
      end
    end)

    ok_count = Enum.count(results, &(&1 == :ok))
    total = length(results)

    IO.puts("\n=== Loaded #{ok_count}/#{total} modules ===\n")

    if ok_count == total do
      IO.puts("Nova is ready! Try:")
      IO.puts("  apply(:\"Nova.Web\", :startServer, [8080])")
      IO.puts("  # Then visit http://localhost:8080")
    end

    :ok
  end
end

# Auto-load on IEx start
Nova.Loader.load_all()
