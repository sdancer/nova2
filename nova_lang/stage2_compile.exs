# Stage 2 Compiler Script
# Compiles PureScript source files using the Nova compiler running on BEAM

defmodule Stage2Compiler do
  def compile(source_path) do
    src = File.read!(source_path)
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
        IO.puts(:stderr, "Usage: mix run stage2_compile.exs <source.purs> <output.ex>")
        System.halt(1)
    end
  end
end

Stage2Compiler.main(System.argv())
