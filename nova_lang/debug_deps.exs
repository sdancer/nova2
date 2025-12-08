# Debug: check what imports are extracted from CstLayout.purs

File.cd!("..")

source = File.read!("./src/Nova/Compiler/CstLayout.purs")
imports = Nova.Compiler.Regenerate.extract_imports(source)
IO.puts("CstLayout imports:")
Enum.each(imports, &IO.puts/1)

IO.puts("\n\nCst imports:")
cst_source = File.read!("./src/Nova/Compiler/Cst.purs")
cst_imports = Nova.Compiler.Regenerate.extract_imports(cst_source)
Enum.each(cst_imports, &IO.puts/1)

# Check dependency graph
IO.puts("\n\n=== Dependency Graph ===")
file_system = %{
  read_file: fn path ->
    case File.read(path) do
      {:ok, content} -> {:just, content}
      {:error, _} -> :nothing
    end
  end,
  write_file: fn path -> fn content -> File.write!(path, content); :unit end end,
  file_exists: &File.exists?/1,
  list_files: fn dir -> fn ext ->
    if File.dir?(dir) do
      Path.wildcard(Path.join([dir, "**", "*#{ext}"]))
      |> Enum.map(fn p -> "./" <> p end)
    else
      []
    end
  end end
}

config = Nova.Compiler.Regenerate.default_config()

# Get compiler files
compiler_files = file_system.list_files.(config.src_base).(".purs")
IO.puts("Compiler files: #{length(compiler_files)}")

# Build dependency graph
dep_graph = Nova.Compiler.Regenerate.build_dependency_graph(file_system, config, compiler_files, false)
IO.puts("\nDependency graph:")
Enum.each(dep_graph, fn {path, deps} ->
  short_name = Path.basename(path, ".purs")
  dep_names = Enum.map(deps, fn d -> Path.basename(d, ".purs") end)
  if length(dep_names) > 0 do
    IO.puts("  #{short_name} -> #{Enum.join(dep_names, ", ")}")
  else
    IO.puts("  #{short_name} -> (none)")
  end
end)

# Topological sort
IO.puts("\n=== Topological Sort ===")
sorted = Nova.Compiler.Regenerate.topological_sort(dep_graph)
IO.puts("Sorted order:")
Enum.with_index(sorted, 1) |> Enum.each(fn {path, idx} ->
  short_name = Path.basename(path, ".purs")
  IO.puts("  #{idx}. #{short_name}")
end)
