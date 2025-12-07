defmodule Nova.CLI do
  @moduledoc """
  Command-line interface for the Nova compiler.
  """

  def main(args) do
    case parse_args(args) do
      {:compile, files, opts} ->
        compile_files(files, opts)

      {:mcp, opts} ->
        start_mcp_server(opts)

      {:help, _} ->
        print_help()

      {:version, _} ->
        IO.puts("Nova Lang v0.1.0 (self-hosted)")

      {:error, msg} ->
        IO.puts(:stderr, "Error: #{msg}")
        System.halt(1)
    end
  end

  defp start_mcp_server(opts) do
    port = Map.get(opts, :port, 9999)
    shared = Map.get(opts, :shared, false)

    # Suppress Elixir logger output to keep MCP protocol clean
    Logger.configure(level: :none)

    # Start multi-connection server
    {:ok, _server} = Nova.MCPMultiServer.start_link(port: port, shared: shared)

    # Keep the process alive
    Process.sleep(:infinity)
  end

  defp parse_args(args) do
    parse_args(args, %{output_dir: ".", files: [], deps: [], port: 9999, shared: false})
  end

  defp parse_args([], acc) do
    if acc.files == [] do
      {:help, acc}
    else
      {:compile, Enum.reverse(acc.files), acc}
    end
  end

  defp parse_args(["mcp" | rest], acc) do
    parse_mcp_args(rest, acc)
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

  defp parse_mcp_args([], acc), do: {:mcp, acc}
  defp parse_mcp_args(["--port", port_str | rest], acc) do
    case Integer.parse(port_str) do
      {port, ""} -> parse_mcp_args(rest, %{acc | port: port})
      _ -> {:error, "Invalid port number: #{port_str}"}
    end
  end
  defp parse_mcp_args(["-p", port_str | rest], acc) do
    parse_mcp_args(["--port", port_str | rest], acc)
  end
  defp parse_mcp_args(["--shared" | rest], acc) do
    parse_mcp_args(rest, %{acc | shared: true})
  end
  defp parse_mcp_args(["-s" | rest], acc) do
    parse_mcp_args(rest, %{acc | shared: true})
  end
  defp parse_mcp_args([unknown | _], _acc) do
    {:error, "Unknown mcp option: #{unknown}"}
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

    Usage: nova [command] [options] <files...>

    Commands:
      mcp [options]       Start MCP (Model Context Protocol) server over TCP

    Compile Options:
      -o, --output DIR    Output directory (default: current directory)
      -d, --dep FILE      Add a dependency file (can be used multiple times)

    MCP Options:
      -p, --port PORT     TCP port to listen on (default: 9999)
      -s, --shared        Share namespaces across all connections (default: isolated)

    General Options:
      -h, --help          Show this help message
      -v, --version       Show version

    Examples:
      nova src/MyModule.purs
      nova -o lib/ src/*.purs
      nova -d src/Types.purs -d src/Ast.purs src/Parser.purs
      nova mcp                              # Start MCP server on port 9999
      nova mcp --port 8080                  # Start MCP server on port 8080
      nova mcp --shared                     # Shared namespace mode
    """)
  end
end
