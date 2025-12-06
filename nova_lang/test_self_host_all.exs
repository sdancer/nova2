# Test self-hosting: compile all compiler modules with the Elixir compiler
# Uses CST pipeline for parsing (same as regenerate.js)

modules = [
  "Ast",
  "Types",
  "Tokenizer",
  "Parser",
  "Unify",
  "TypeChecker",
  "CodeGen",
  "Dependencies",
  "Cst",
  "CstPipeline",
  "CstLayout",
  "CstLexer",
  "CstParser",
  "CstToAst",
]

results = Enum.map(modules, fn mod ->
  path = "../src/Nova/Compiler/#{mod}.purs"
  IO.write("Testing #{mod}... ")

  case File.read(path) do
    {:error, reason} ->
      IO.puts("SKIP (file not found: #{reason})")
      {:skip, mod}
    {:ok, source} ->
      # Use CST pipeline (same as regenerate.js)
      case Nova.Compiler.CstPipeline.parse_module_cst(source) do
        {:left, err} ->
          IO.puts("PARSE ERROR: #{inspect(err)}")
          {:parse_error, mod, err}
        {:right, ast} ->
          # Generate code (gen_module returns a function that takes the AST)
          code = Nova.Compiler.CodeGen.gen_module().(ast)
          lines = String.split(code, "\n") |> length()

          # Compare with existing
          existing_path = "lib/nova/compiler/#{mod}.ex"
          case File.read(existing_path) do
            {:error, _} ->
              IO.puts("OK (#{lines} lines, no existing file to compare)")
              {:ok, mod, lines, :no_compare}
            {:ok, existing} ->
              existing_lines = String.split(existing, "\n") |> length()
              if code == existing do
                IO.puts("EXACT MATCH (#{lines} lines)")
                {:ok, mod, lines, :exact}
              else
                # Check if semantically same by normalizing whitespace
                normalize = fn s ->
                  s |> String.split("\n")
                    |> Enum.reject(&(String.trim(&1) == ""))
                    |> Enum.join("\n")
                end
                if normalize.(code) == normalize.(existing) do
                  IO.puts("OK - whitespace diff (#{lines} vs #{existing_lines} lines)")
                  {:ok, mod, lines, :whitespace_diff}
                else
                  IO.puts("DIFFERS (#{lines} vs #{existing_lines} lines)")
                  {:differs, mod, lines, existing_lines}
                end
              end
          end
      end
  end
end)

IO.puts("\n=== Summary ===")
ok = Enum.filter(results, fn {status, _, _, _} -> status == :ok end) |> length()
exact = Enum.filter(results, fn r -> match?({:ok, _, _, :exact}, r) end) |> length()
differs = Enum.filter(results, fn r -> match?({:differs, _, _, _}, r) end) |> length()
errors = Enum.filter(results, fn r -> match?({:parse_error, _, _}, r) end) |> length()
skipped = Enum.filter(results, fn r -> match?({:skip, _}, r) end) |> length()

IO.puts("Passed: #{ok}/#{length(modules)} (#{exact} exact matches)")
IO.puts("Differs: #{differs}")
IO.puts("Parse errors: #{errors}")
IO.puts("Skipped: #{skipped}")

if differs > 0 do
  IO.puts("\nModules with differences:")
  Enum.filter(results, fn r -> match?({:differs, _, _, _}, r) end)
  |> Enum.each(fn {:differs, mod, gen, exist} ->
    IO.puts("  #{mod}: generated #{gen} lines, existing #{exist} lines")
  end)
end
