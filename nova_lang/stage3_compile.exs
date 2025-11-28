# Stage 3 Compiler Script
# Uses Stage 2 compiled modules to compile PureScript source
# This proves the compiler can fully self-host

defmodule Stage3Compiler do
  @stage2_dir "../output/stage2"

  def load_stage2_modules do
    # The Stage 2 modules need to be loaded dynamically
    # They're .ex files that define Nova.Compiler.* modules
    modules = [
      "Ast",
      "Types",
      "Tokenizer",
      "Unify",
      "TypeChecker",
      "CodeGen",
      "Parser",
      "Dependencies"
    ]

    for mod <- modules do
      path = Path.join(@stage2_dir, "#{mod}.ex")
      if File.exists?(path) do
        Code.compile_file(path)
      else
        IO.puts(:stderr, "Warning: Stage 2 module not found: #{path}")
      end
    end
  end

  def compile(source_path) do
    src = File.read!(source_path)

    # Use the dynamically loaded Stage 2 modules
    # After loading, they replace the existing Nova.Compiler.* modules
    tokens = Nova.Compiler.Tokenizer.tokenize(src)

    case Nova.Compiler.Parser.parse_module(tokens) do
      {:left, err} ->
        IO.puts(:stderr, "Parse error: #{inspect(err)}")
        nil
      {:right, {:tuple, mod, _rest}} ->
        Nova.Compiler.CodeGen.gen_module(mod)
    end
  end

  def main(args) do
    case args do
      ["--load-stage2" | rest] ->
        IO.puts(:stderr, "Loading Stage 2 modules...")
        load_stage2_modules()
        IO.puts(:stderr, "Stage 2 modules loaded.")
        main(rest)

      [source_path, output_path] ->
        case compile(source_path) do
          nil ->
            System.halt(1)
          code ->
            File.write!(output_path, code)
            lines = code |> String.split("\n") |> length()
            IO.puts(:stderr, "Generated #{lines} lines -> #{output_path}")
        end

      _ ->
        IO.puts(:stderr, "Usage: mix run stage3_compile.exs [--load-stage2] <source.purs> <output.ex>")
        System.halt(1)
    end
  end
end

Stage3Compiler.main(System.argv())
