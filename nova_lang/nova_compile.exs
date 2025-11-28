# Nova Multi-Module Compiler
# Compiles multiple PureScript files with import/export resolution

defmodule NovaCompile do
  def main(args) do
    {opts, paths} = parse_args(args)

    case paths do
      [] ->
        IO.puts(:stderr, """
        Nova Compiler

        Usage: mix run nova_compile.exs [options] <source.purs> [source2.purs ...]

        Options:
          --check       Enable type checking
          --output DIR  Output directory (default: current directory)

        Examples:
          # Compile single file
          mix run nova_compile.exs src/Ast.purs

          # Compile multiple files with type checking
          mix run nova_compile.exs --check src/*.purs

          # Compile to specific directory
          mix run nova_compile.exs --output lib/ src/*.purs
        """)
        System.halt(1)

      [source_path] when length(paths) == 1 ->
        # Single file mode - use simple compiler
        compile_single(source_path, opts)

      _ ->
        # Multi-file mode - use module compiler
        compile_multi(paths, opts)
    end
  end

  defp compile_single(source_path, opts) do
    check = Keyword.get(opts, :check, false)
    output_dir = Keyword.get(opts, :output, ".")

    src = File.read!(source_path)
    tokens = Nova.Compiler.Tokenizer.tokenize(src)

    case Nova.Compiler.Parser.parse_module(tokens) do
      {:left, err} ->
        IO.puts(:stderr, "Parse error: #{inspect(err)}")
        System.halt(1)

      {:right, {:tuple, mod, _rest}} ->
        code = if check do
          env = Nova.Compiler.Types.empty_env()
          case Nova.Compiler.TypeChecker.check_module(env, mod.declarations) do
            {:left, err} ->
              IO.puts(:stderr, "Type error: #{inspect(err)}")
              System.halt(1)
            {:right, _} ->
              Nova.Compiler.CodeGen.gen_module(mod)
          end
        else
          Nova.Compiler.CodeGen.gen_module(mod)
        end

        output_path = Path.join(output_dir, module_to_filename(mod.name))
        File.write!(output_path, code)
        lines = code |> String.split("\n") |> length()
        IO.puts(:stderr, "Generated #{lines} lines -> #{output_path}")
    end
  end

  defp compile_multi(paths, opts) do
    check = Keyword.get(opts, :check, false)
    output_dir = Keyword.get(opts, :output, ".")

    case Nova.ModuleCompiler.compile_all(paths, check: check, output_dir: output_dir) do
      {:ok, outputs} ->
        IO.puts(:stderr, "\nSuccessfully compiled #{length(outputs)} modules")

      {:error, msg} ->
        IO.puts(:stderr, "Error: #{msg}")
        System.halt(1)
    end
  end

  defp parse_args(args) do
    Enum.reduce(args, {[], []}, fn
      "--check", {opts, paths} ->
        {Keyword.put(opts, :check, true), paths}

      "--output", {opts, paths} ->
        {Keyword.put(opts, :next_is_output, true), paths}

      arg, {opts, paths} ->
        if Keyword.get(opts, :next_is_output) do
          {opts |> Keyword.delete(:next_is_output) |> Keyword.put(:output, arg), paths}
        else
          {opts, paths ++ [arg]}
        end
    end)
  end

  defp module_to_filename(module_name) do
    module_name
    |> String.split(".")
    |> List.last()
    |> Kernel.<>(".ex")
  end
end

NovaCompile.main(System.argv())
