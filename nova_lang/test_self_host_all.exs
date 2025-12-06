# Test self-hosting: compile all compiler modules with the Elixir compiler
# Parses source, generates code, and compares with existing generated files
# NOTE: Type checking is skipped because the generated TypeChecker code has
# curried function calls that don't work at runtime in Elixir (yet)

alias Nova.Compiler.{CstPipeline, CodeGen}

# Compiler modules to test
compiler_modules = [
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

IO.puts("=== Testing Compiler Modules (Parse + CodeGen only) ===")
IO.puts("Note: Type checking skipped due to curried function call issue\n")

# Parse a module (no type checking)
defmodule SelfHostTest do
  def parse_only(path) do
    case File.read(path) do
      {:error, reason} -> {:error, "file not found: #{reason}"}
      {:ok, source} ->
        case CstPipeline.parse_module_cst(source) do
          {:left, err} -> {:error, "parse error: #{inspect(err)}"}
          {:right, ast} -> {:ok, ast}
        end
    end
  end
end

results = Enum.map(compiler_modules, fn mod ->
  path = "../src/Nova/Compiler/#{mod}.purs"
  IO.write("Testing #{mod}... ")

  case SelfHostTest.parse_only(path) do
    {:error, reason} ->
      IO.puts("ERROR: #{reason}")
      {:error, mod, reason}
    {:ok, ast} ->
      # Generate code WITHOUT type environment (Nothing)
      code = CodeGen.gen_module_with_env(:nothing, ast)
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
end)

IO.puts("\n=== Summary ===")
ok = Enum.filter(results, fn r -> match?({:ok, _, _, _}, r) end) |> length()
exact = Enum.filter(results, fn r -> match?({:ok, _, _, :exact}, r) end) |> length()
differs = Enum.filter(results, fn r -> match?({:differs, _, _, _}, r) end) |> length()
errors = Enum.filter(results, fn r -> match?({:error, _, _}, r) end) |> length()

IO.puts("Passed: #{ok}/#{length(compiler_modules)} (#{exact} exact matches)")
IO.puts("Differs: #{differs}")
IO.puts("Errors: #{errors}")

if differs > 0 do
  IO.puts("\nModules with differences:")
  Enum.filter(results, fn r -> match?({:differs, _, _, _}, r) end)
  |> Enum.each(fn {:differs, mod, gen, exist} ->
    IO.puts("  #{mod}: generated #{gen} lines, existing #{exist} lines")
  end)
end

if errors > 0 do
  IO.puts("\nModules with errors:")
  Enum.filter(results, fn r -> match?({:error, _, _}, r) end)
  |> Enum.each(fn {:error, mod, reason} ->
    IO.puts("  #{mod}: #{reason}")
  end)
end
