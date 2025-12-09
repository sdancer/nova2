# Test the Elixir compiler regenerating itself using Regenerate module
# This tests the full self-hosting pipeline

IO.puts("=== Testing Nova Self-Hosting via Regenerate ===\n")

# Create filesystem delegates for the Regenerate module
fs = %{
  read_file: fn path ->
    case File.read(path) do
      {:ok, content} -> {:just, content}
      {:error, _} -> :nothing
    end
  end,
  write_file: fn path ->
    fn content ->
      File.write!(path, content)
      :unit
    end
  end,
  list_files: fn dir ->
    fn ext ->
      IO.puts("DEBUG: Listing files in #{dir} with extension #{ext}")
      case File.ls(dir) do
        {:ok, files} ->
          result = files
          |> Enum.filter(&String.ends_with?(&1, ext))
          |> Enum.map(&Path.join(dir, &1))
          |> Enum.sort()
          IO.puts("DEBUG: Found #{length(result)} files: #{inspect(result)}")
          result
        {:error, reason} ->
          IO.puts("DEBUG: Error listing #{dir}: #{reason}")
          []
      end
    end
  end,
  file_exists: fn path ->
    File.exists?(path)
  end
}

# Config for regenerate - paths relative to nova2 directory
cfg = %{
  lib_base: "../lib/",
  src_base: "../src/Nova/Compiler/",
  output_dir: "./output_test/",
  target_dir: "./output_test2/"
}

# Create output directories
File.mkdir_p!(cfg.output_dir)
File.mkdir_p!(cfg.target_dir)

# Run regenerate
IO.puts("Starting regeneration...")
IO.puts("lib_base: #{cfg.lib_base}")
IO.puts("src_base: #{cfg.src_base}")
IO.puts("")

try do
  result = Nova.Compiler.Regenerate.regenerate(fs, cfg)

  IO.puts("\n=== Result ===")
  IO.puts("Success: #{result.success}")
  IO.puts("Modules compiled: #{result.modules_compiled}")

  IO.puts("\n=== Logs ===")
  Enum.each(result.logs, fn log ->
    IO.puts(Nova.Compiler.Regenerate.show_log_entry(log))
  end)
rescue
  e ->
    IO.puts("\n=== Error ===")
    IO.puts(Exception.message(e))
    IO.puts(Exception.format_stacktrace(__STACKTRACE__))
end
