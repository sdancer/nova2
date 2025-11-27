# Stage 2 Compiler Script
# Compiles PureScript source files using the Nova compiler running on BEAM
# Supports optional type checking with --check flag

defmodule Stage2Compiler do
  def compile(source_path, opts \\ []) do
    src = File.read!(source_path)
    # Use fast native tokenizer instead of transpiled one
    tokens = Nova.FastTokenizer.tokenize(src)

    case Nova.Compiler.Parser.parse_module(tokens) do
      {:left, err} ->
        {:error, "Parse error: #{inspect(err)}"}

      {:right, {:tuple, mod, _rest}} ->
        if opts[:check] do
          # Run type checker before code generation
          env = Nova.Compiler.Types.empty_env()
          case Nova.Compiler.TypeChecker.check_module(env, mod.declarations) do
            {:left, err} ->
              {:error, "Type error: #{inspect(err)}"}
            {:right, _typed_env} ->
              {:ok, Nova.Compiler.CodeGen.gen_module(mod)}
          end
        else
          {:ok, Nova.Compiler.CodeGen.gen_module(mod)}
        end
    end
  end

  def main(args) do
    {opts, paths} = parse_args(args)

    case paths do
      [source_path, output_path] ->
        case compile(source_path, opts) do
          {:error, msg} ->
            IO.puts(:stderr, msg)
            System.halt(1)
          {:ok, code} ->
            File.write!(output_path, code)
            lines = code |> String.split("\n") |> length()
            IO.puts(:stderr, "Generated #{lines} lines -> #{output_path}")
        end
      _ ->
        IO.puts(:stderr, "Usage: mix run stage2_compile.exs [--check] <source.purs> <output.ex>")
        System.halt(1)
    end
  end

  defp parse_args(args) do
    Enum.reduce(args, {[], []}, fn
      "--check", {opts, paths} -> {Keyword.put(opts, :check, true), paths}
      path, {opts, paths} -> {opts, paths ++ [path]}
    end)
  end
end

Stage2Compiler.main(System.argv())
