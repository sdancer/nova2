# Self-compilation test: Compile all 8 modules using the BEAM compiler
# This verifies the compiled Elixir can compile its own source

defmodule SelfCompileTest do
  @source_dir "../src/Nova/Compiler"
  @output_dir "lib/nova/compiler"

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
    compare = "--compare" in args

    IO.puts("=== Self-Compilation Test ===")
    IO.puts("Using BEAM-compiled Nova to compile its own source\n")

    results = Enum.map(@modules, fn mod_name ->
      path = Path.join(@source_dir, "#{mod_name}.purs")
      IO.write("Compiling #{mod_name}... ")

      src = File.read!(path)
      tokens = Nova.Compiler.Tokenizer.tokenize(src)

      case Nova.Compiler.Parser.parse_module(tokens) do
        {:right, {:tuple, mod, _}} ->
          code = Nova.Compiler.CodeGen.gen_module(mod)
          lines = length(String.split(code, "\n"))

          if compare do
            js_path = Path.join(@output_dir, "#{mod_name}.ex")
            js_code = File.read!(js_path)
            if code == js_code do
              IO.puts("OK (#{lines} lines, matches JS)")
            else
              IO.puts("OK (#{lines} lines, DIFFERS from JS)")
            end
          else
            IO.puts("OK (#{lines} lines)")
          end

          {:ok, mod_name, code}
        {:left, err} ->
          IO.puts("FAILED - #{inspect(err)}")
          {:error, mod_name, err}
      end
    end)

    ok_count = Enum.count(results, fn {status, _, _} -> status == :ok end)
    IO.puts("\n=== Results: #{ok_count}/#{length(@modules)} modules compiled ===")

    if ok_count == length(@modules) do
      IO.puts("\nSelf-compilation successful!")
    end
  end
end

SelfCompileTest.main(System.argv())
