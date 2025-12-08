# Test the Regenerate module from Elixir
# This creates filesystem delegates and calls the compiled Nova regenerate function

# Change to project root since paths are relative to there
File.cd!("..")
IO.puts("Working directory: #{File.cwd!()}")

IO.puts("=== Nova Compiler Regeneration (Elixir) ===\n")

# Create filesystem delegates that match the FileSystem type
file_system = %{
  read_file: fn path ->
    case File.read(path) do
      {:ok, content} -> {:just, content}
      {:error, _} -> :nothing
    end
  end,

  write_file: fn path ->
    fn content ->
      # Ensure directory exists
      path |> Path.dirname() |> File.mkdir_p!()
      File.write!(path, content)
      :unit
    end
  end,

  file_exists: fn path ->
    File.exists?(path)
  end,

  # list_files is called as list_files.(dir, ext) - non-curried style
  list_files: fn dir, extension ->
    if File.dir?(dir) do
      Path.wildcard(Path.join([dir, "**", "*#{extension}"]))
      |> Enum.map(fn p -> "./" <> p end)
    else
      []
    end
  end
}

# Use default config
config = Nova.Compiler.Regenerate.default_config()

IO.puts("Source dir: #{config.src_base}")
IO.puts("Library dir: #{config.lib_base}")
IO.puts("Output dir: #{config.output_dir}")
IO.puts("Target dir: #{config.target_dir}")
IO.puts("")

# Test list_files first
lib_files = file_system.list_files.("./lib/Data", ".purs")
IO.puts("Library files found: #{length(lib_files)}")
Enum.take(lib_files, 5) |> Enum.each(&IO.puts/1)

compiler_files = file_system.list_files.("./src/Nova/Compiler/", ".purs")
IO.puts("\nCompiler files found: #{length(compiler_files)}")
Enum.take(compiler_files, 5) |> Enum.each(&IO.puts/1)

IO.puts("\n--- Running regenerate ---\n")

# Run regeneration
result = Nova.Compiler.Regenerate.regenerate(file_system, config)

IO.puts("Logs count: #{length(result.logs)}")
IO.inspect(result.logs, label: "Logs")

# Print logs
IO.puts("\nFormatted logs:")
IO.puts(Nova.Compiler.Regenerate.show_logs().(result.logs))

# Print summary
IO.puts("\n=== Summary ===")
IO.puts("Modules compiled: #{result.modules_compiled}")
IO.puts("Success: #{result.success}")
