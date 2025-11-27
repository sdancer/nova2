defmodule Nova.CLI do
  @moduledoc """
  Command-line interface for the Nova compiler.
  """

  def main(args) do
    case parse_args(args) do
      {:compile, files, opts} ->
        compile_files(files, opts)

      {:help, _} ->
        print_help()

      {:version, _} ->
        IO.puts("Nova Lang v0.1.0 (self-hosted)")

      {:error, msg} ->
        IO.puts(:stderr, "Error: #{msg}")
        System.halt(1)
    end
  end

  defp parse_args(args) do
    parse_args(args, %{output_dir: ".", files: [], deps: []})
  end

  defp parse_args([], acc) do
    if acc.files == [] do
      {:help, acc}
    else
      {:compile, Enum.reverse(acc.files), acc}
    end
  end

  defp parse_args(["--help" | _], acc), do: {:help, acc}
  defp parse_args(["-h" | _], acc), do: {:help, acc}
  defp parse_args(["--version" | _], acc), do: {:version, acc}
  defp parse_args(["-v" | _], acc), do: {:version, acc}

  defp parse_args(["--output", dir | rest], acc) do
    parse_args(rest, %{acc | output_dir: dir})
  end

  defp parse_args(["-o", dir | rest], acc) do
    parse_args(rest, %{acc | output_dir: dir})
  end

  defp parse_args(["--dep", dep | rest], acc) do
    parse_args(rest, %{acc | deps: [dep | acc.deps]})
  end

  defp parse_args(["-d", dep | rest], acc) do
    parse_args(rest, %{acc | deps: [dep | acc.deps]})
  end

  defp parse_args(["-" <> _ = unknown | _], _acc) do
    {:error, "Unknown option: #{unknown}"}
  end

  defp parse_args([file | rest], acc) do
    parse_args(rest, %{acc | files: [file | acc.files]})
  end

  defp compile_files(files, opts) do
    output_dir = opts.output_dir
    File.mkdir_p!(output_dir)

    # Load dependency sources
    dep_sources = Enum.map(Enum.reverse(opts.deps), fn dep ->
      case File.read(dep) do
        {:ok, src} -> src
        {:error, _} ->
          IO.puts(:stderr, "Warning: Could not read dependency #{dep}")
          ""
      end
    end)

    results = Enum.map(files, fn file ->
      IO.puts("Compiling #{file}...")

      case compile_with_deps(file, dep_sources) do
        {:ok, code} ->
          basename = Path.basename(file, ".purs")
          output_path = Path.join(output_dir, "#{basename}.ex")
          File.write!(output_path, code)
          IO.puts("  -> #{output_path}")
          :ok

        {:error, {:parse, err}} ->
          IO.puts(:stderr, "  Parse error: #{inspect(err)}")
          :error

        {:error, {:typecheck, err}} ->
          IO.puts(:stderr, "  Type error: #{inspect(err)}")
          :error

        {:error, reason} ->
          IO.puts(:stderr, "  Error: #{inspect(reason)}")
          :error
      end
    end)

    errors = Enum.count(results, &(&1 == :error))

    if errors > 0 do
      IO.puts(:stderr, "\n#{errors} file(s) failed to compile")
      System.halt(1)
    else
      IO.puts("\nCompiled #{length(files)} file(s) successfully")
    end
  end

  defp compile_with_deps(path, dep_sources) do
    case File.read(path) do
      {:ok, source} ->
        Nova.compile(source, dep_sources)
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp print_help do
    IO.puts("""
    Nova Lang Compiler v0.1.0

    Usage: nova [options] <files...>

    Options:
      -o, --output DIR    Output directory (default: current directory)
      -d, --dep FILE      Add a dependency file (can be used multiple times)
      -h, --help          Show this help message
      -v, --version       Show version

    Examples:
      nova src/MyModule.purs
      nova -o lib/ src/*.purs
      nova -d src/Types.purs -d src/Ast.purs src/Parser.purs
    """)
  end
end
