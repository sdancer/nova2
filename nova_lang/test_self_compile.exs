source = File.read!("../src/Nova/Compiler/Ast.purs")
IO.puts("Compiling Ast.purs (#{String.length(source)} chars)...")

result = Nova.compile(source)
case result do
  {:ok, code} ->
    IO.puts("SUCCESS: #{String.length(code)} chars generated")
    IO.puts("\nFirst 800 chars:")
    IO.puts(String.slice(code, 0, 800))
  {:error, err} ->
    IO.puts("ERROR: #{inspect(err, limit: 500, pretty: true)}")
end
