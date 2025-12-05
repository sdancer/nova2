modules = [
  "Ast",
  "Types",
  "Tokenizer",
  "Parser",
  "Unify",
  "TypeChecker",
  "CodeGen",
  "CodeGenCoreErlang",
  "CodeGenWasm",
  "CodeGenWasmSimple",
  "Dependencies",
  "Cst",
  "CstLayout",
  "CstLexer",
  "CstParser",
  "CstPipeline",
  "CstToAst",
  "RefEq"
]

results = Enum.map(modules, fn mod ->
  path = "../src/Nova/Compiler/#{mod}.purs"
  case File.read(path) do
    {:ok, source} ->
      result = Nova.compile(source)
      case result do
        {:ok, code} ->
          {:ok, mod, String.length(source), String.length(code)}
        {:error, err} ->
          {:error, mod, err}
      end
    {:error, reason} ->
      {:file_error, mod, reason}
  end
end)

IO.puts("\n=== Self-Compilation Test Results ===\n")

successes = Enum.filter(results, fn
  {:ok, _, _, _} -> true
  _ -> false
end)

failures = Enum.filter(results, fn
  {:ok, _, _, _} -> false
  _ -> true
end)

Enum.each(successes, fn {:ok, mod, src_len, out_len} ->
  IO.puts("✓ #{mod}.purs: #{src_len} chars → #{out_len} chars")
end)

Enum.each(failures, fn
  {:error, mod, err} ->
    IO.puts("✗ #{mod}.purs: #{inspect(err, limit: 100)}")
  {:file_error, mod, reason} ->
    IO.puts("✗ #{mod}.purs: File error - #{reason}")
end)

IO.puts("\n#{length(successes)}/#{length(modules)} modules compiled successfully")
