# Self-hosting test: Compile ALL compiler modules using the self-hosted compiler
# This verifies the compiled Core Erlang compiler can compile its own source

IO.puts("=== Nova Full Self-Hosting Test ===\n")

# Directory containing compiled .core files
core_dir = "lib/nova"

# Load a .core file and compile it into the BEAM VM
load_core_module = fn path ->
  case File.read(path) do
    {:ok, content} ->
      case Nova.CoreErlangEval.compile_and_load(content) do
        {:ok, mod_name} ->
          {:ok, mod_name}
        {:error, reason} ->
          {:error, path, reason}
      end
    {:error, reason} ->
      {:error, path, reason}
  end
end

# Load modules in dependency order
lib_modules = [
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
  "Nova/Prelude.core"
]

compiler_modules = [
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
  "Nova/Compiler/Regenerate.core"
]

all_modules = lib_modules ++ compiler_modules

IO.puts("Loading #{length(all_modules)} Core Erlang modules...\n")

# Load all modules
results = Enum.map(all_modules, fn rel_path ->
  path = Path.join(core_dir, rel_path)
  IO.write("  Loading #{rel_path}... ")
  case load_core_module.(path) do
    {:ok, mod_name} ->
      IO.puts("OK (#{mod_name})")
      {:ok, mod_name}
    {:error, path, reason} ->
      IO.puts("FAILED: #{inspect(reason)}")
      {:error, path, reason}
  end
end)

loaded = Enum.count(results, fn
  {:ok, _} -> true
  _ -> false
end)
failed = Enum.count(results, fn
  {:error, _, _} -> true
  _ -> false
end)

IO.puts("\n=== Module Loading Summary ===")
IO.puts("Loaded: #{loaded}/#{length(all_modules)}")

if failed > 0 do
  IO.puts("Failed: #{failed}")
  IO.puts("\nCannot proceed - modules failed to load")
  System.halt(1)
else
  IO.puts("\n✓ All Core Erlang modules loaded successfully!")
end

# Source files to compile (compiler sources)
src_files = [
  {"src/Nova/Compiler/Ast.purs", "Nova.Compiler.Ast"},
  {"src/Nova/Compiler/Types.purs", "Nova.Compiler.Types"},
  {"src/Nova/Compiler/Cst.purs", "Nova.Compiler.Cst"},
  {"src/Nova/Compiler/CstLayout.purs", "Nova.Compiler.CstLayout"},
  {"src/Nova/Compiler/CstLexer.purs", "Nova.Compiler.CstLexer"},
  {"src/Nova/Compiler/CstParser.purs", "Nova.Compiler.CstParser"},
  {"src/Nova/Compiler/CstToAst.purs", "Nova.Compiler.CstToAst"},
  {"src/Nova/Compiler/CstPipeline.purs", "Nova.Compiler.CstPipeline"},
  {"src/Nova/Compiler/Unify.purs", "Nova.Compiler.Unify"},
  {"src/Nova/Compiler/ImportProcessor.purs", "Nova.Compiler.ImportProcessor"},
  {"src/Nova/Compiler/TypeChecker.purs", "Nova.Compiler.TypeChecker"},
  {"src/Nova/Compiler/RefEq.purs", "Nova.Compiler.RefEq"},
  {"src/Nova/Compiler/Dependencies.purs", "Nova.Compiler.Dependencies"},
  {"src/Nova/Compiler/CodeGenCoreErlang.purs", "Nova.Compiler.CodeGenCoreErlang"},
  {"src/Nova/Compiler/Regenerate.purs", "Nova.Compiler.Regenerate"}
]

IO.puts("\n=== Compiling Compiler Sources ===\n")

compile_file = fn {path, expected_name} ->
  full_path = Path.join("..", path)
  IO.write("  Compiling #{expected_name}... ")

  case File.read(full_path) do
    {:ok, source} ->
      source_charlist = String.to_charlist(source)

      # Parse using self-hosted compiler
      case apply(:"Nova.Compiler.CstPipeline", :parseModuleCst, [source_charlist]) do
        {:Left, error} ->
          IO.puts("PARSE ERROR")
          {:error, expected_name, {:parse, error}}

        {:Right, mod} ->
          # Generate Core Erlang
          try do
            code = apply(:"Nova.Compiler.CodeGenCoreErlang", :genModule, [mod])
            code_str = List.to_string(code)
            IO.puts("OK (#{String.length(code_str)} chars)")
            {:ok, expected_name, code_str}
          rescue
            e ->
              IO.puts("CODEGEN ERROR: #{Exception.message(e)}")
              {:error, expected_name, {:codegen, Exception.message(e)}}
          end
      end

    {:error, reason} ->
      IO.puts("FILE ERROR: #{inspect(reason)}")
      {:error, expected_name, {:file, reason}}
  end
end

compile_results = Enum.map(src_files, compile_file)

compiled = Enum.count(compile_results, fn
  {:ok, _, _} -> true
  _ -> false
end)
compile_failed = Enum.count(compile_results, fn
  {:error, _, _} -> true
  _ -> false
end)

IO.puts("\n=== Compilation Summary ===")
IO.puts("Compiled: #{compiled}/#{length(src_files)}")

if compile_failed > 0 do
  IO.puts("Failed: #{compile_failed}")
  IO.puts("\nFailed modules:")
  Enum.each(compile_results, fn
    {:error, name, reason} ->
      IO.puts("  - #{name}: #{inspect(reason)}")
    _ -> :ok
  end)
  IO.puts("\n=== Self-Hosting Test FAILED ===")
  System.halt(1)
else
  IO.puts("\n=== Full Self-Hosting Test PASSED ===")
  IO.puts("All #{length(src_files)} compiler modules compiled successfully!")
end
