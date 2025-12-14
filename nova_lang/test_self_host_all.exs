# Self-hosting test: Compile ALL compiler modules using the self-hosted compiler
# This verifies the compiled Core Erlang compiler can compile its own source

IO.puts("=== Nova Full Self-Hosting Test ===\n")

# Helper to format time in milliseconds
format_time = fn microseconds ->
  ms = microseconds / 1000
  if ms >= 1000, do: "#{Float.round(ms / 1000, 2)}s", else: "#{Float.round(ms, 1)}ms"
end

# Directory containing compiled .core files
core_dir = "priv/core"

# Load a .core file and compile it into the BEAM VM
load_core_module = fn path ->
  case File.read(path) do
    {:ok, content} ->
      {time, result} = :timer.tc(fn -> Nova.CoreErlangEval.compile_and_load(content) end)
      case result do
        {:ok, mod_name} ->
          {:ok, mod_name, time}
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
load_start = System.monotonic_time(:microsecond)
results = Enum.map(all_modules, fn rel_path ->
  path = Path.join(core_dir, rel_path)
  IO.write("  Loading #{rel_path}... ")
  case load_core_module.(path) do
    {:ok, mod_name, time} ->
      IO.puts("OK (#{mod_name}) [#{format_time.(time)}]")
      {:ok, mod_name, time}
    {:error, path, reason} ->
      IO.puts("FAILED: #{inspect(reason)}")
      {:error, path, reason}
  end
end)
load_total = System.monotonic_time(:microsecond) - load_start

loaded = Enum.count(results, fn
  {:ok, _, _} -> true
  _ -> false
end)
failed = Enum.count(results, fn
  {:error, _, _} -> true
  _ -> false
end)

IO.puts("\n=== Module Loading Summary ===")
IO.puts("Loaded: #{loaded}/#{length(all_modules)} in #{format_time.(load_total)}")

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
  IO.write("  #{expected_name}... ")

  case File.read(full_path) do
    {:ok, source} ->
      # Phase 1: Lexing (tokenization)
      {lex_time, tokens} = :timer.tc(fn ->
        apply(:"Nova.Compiler.CstLexer", :lexModule, [source])
      end)

      # Phase 2: CST parsing
      {cst_time, cst_result} = :timer.tc(fn ->
        apply(:"Nova.Compiler.CstParser", :runParser, [
          apply(:"Nova.Compiler.CstParser", :parseModule, []),
          tokens
        ])
      end)

      case cst_result do
        {:Left, error} ->
          IO.puts("PARSE ERROR [lex: #{format_time.(lex_time)}, cst: #{format_time.(cst_time)}]")
          {:error, expected_name, {:parse, error}, lex_time, cst_time, 0, 0}

        {:Right, {:Tuple, cst_mod, _rest}} ->
          # Phase 3: CST to AST conversion
          {ast_time, ast_result} = :timer.tc(fn ->
            apply(:"Nova.Compiler.CstToAst", :convertModule, [cst_mod])
          end)

          case ast_result do
            {:Left, error} ->
              IO.puts("AST ERROR [lex: #{format_time.(lex_time)}, cst: #{format_time.(cst_time)}, ast: #{format_time.(ast_time)}]")
              {:error, expected_name, {:ast, error}, lex_time, cst_time, ast_time, 0}

            {:Right, mod} ->
              # Phase 4: Generate Core Erlang
              try do
                {codegen_time, codegen_result} = :timer.tc(fn ->
                  apply(:"Nova.Compiler.CodeGenCoreErlang", :genModule, [mod])
                end)
                case codegen_result do
                  {:Left, error} ->
                    error_str = if is_binary(error), do: error, else: inspect(error)
                    IO.puts("CODEGEN ERROR [lex: #{format_time.(lex_time)}, cst: #{format_time.(cst_time)}, ast: #{format_time.(ast_time)}]: #{error_str}")
                    {:error, expected_name, {:codegen, error_str}, lex_time, cst_time, ast_time, 0}
                  {:Right, code} ->
                    code_str = if is_binary(code), do: code, else: List.to_string(code)
                    IO.puts("OK [lex: #{format_time.(lex_time)}, cst: #{format_time.(cst_time)}, ast: #{format_time.(ast_time)}, codegen: #{format_time.(codegen_time)}]")
                    {:ok, expected_name, code_str, lex_time, cst_time, ast_time, codegen_time}
                end
              rescue
                e ->
                  IO.puts("CODEGEN ERROR: #{Exception.message(e)}")
                  {:error, expected_name, {:codegen, Exception.message(e)}, lex_time, cst_time, ast_time, 0}
              end
          end
      end

    {:error, reason} ->
      IO.puts("FILE ERROR: #{inspect(reason)}")
      {:error, expected_name, {:file, reason}, 0, 0, 0, 0}
  end
end

compile_start = System.monotonic_time(:microsecond)
compile_results = Enum.map(src_files, compile_file)
compile_total = System.monotonic_time(:microsecond) - compile_start

compiled = Enum.count(compile_results, fn
  {:ok, _, _, _, _, _, _} -> true
  _ -> false
end)
compile_failed = Enum.count(compile_results, fn
  {:error, _, _, _, _, _, _} -> true
  _ -> false
end)

# Calculate total times for each phase
{total_lex, total_cst, total_ast, total_codegen} = Enum.reduce(compile_results, {0, 0, 0, 0}, fn
  {:ok, _, _, lex, cst, ast, codegen}, {l, c, a, g} -> {l + lex, c + cst, a + ast, g + codegen}
  {:error, _, _, lex, cst, ast, codegen}, {l, c, a, g} -> {l + lex, c + cst, a + ast, g + codegen}
end)

IO.puts("\n=== Compilation Summary ===")
IO.puts("Compiled: #{compiled}/#{length(src_files)} in #{format_time.(compile_total)}")
IO.puts("  Lexing (tokenize):  #{format_time.(total_lex)}")
IO.puts("  CST parsing:        #{format_time.(total_cst)}")
IO.puts("  CST -> AST:         #{format_time.(total_ast)}")
IO.puts("  CodeGen:            #{format_time.(total_codegen)}")

if compile_failed > 0 do
  IO.puts("Failed: #{compile_failed}")
  IO.puts("\nFailed modules:")
  Enum.each(compile_results, fn
    {:error, name, reason, _, _, _, _} ->
      IO.puts("  - #{name}: #{inspect(reason)}")
    _ -> :ok
  end)
  IO.puts("\n=== Self-Hosting Test FAILED ===")
  System.halt(1)
else
  IO.puts("\n=== Full Self-Hosting Test PASSED ===")
  IO.puts("All #{length(src_files)} compiler modules compiled successfully!")
end
