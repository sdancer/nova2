# Import and compile Nova compiler source files
# Run with: mix run import_compiler.exs

defmodule CompilerImporter do
  @base_path "/home/sdancer/nova2/src/Nova/Compiler"
  @compiler_files [
    {"Nova.Compiler.Ast", "Ast.purs"},
    {"Nova.Compiler.Types", "Types.purs"},
    {"Nova.Compiler.Tokenizer", "Tokenizer.purs"},
    {"Nova.Compiler.Unify", "Unify.purs"},
    {"Nova.Compiler.TypeChecker", "TypeChecker.purs"},
    {"Nova.Compiler.CodeGen", "CodeGen.purs"},
    {"Nova.Compiler.Parser", "Parser.purs"},
    {"Nova.Compiler.Dependencies", "Dependencies.purs"},
  ]

  def run do
    IO.puts("=== Nova Compiler Import Test ===\n")

    # Start the NamespaceService
    {:ok, svc} = Nova.NamespaceService.start_link()

    # Import each file
    results = Enum.map(@compiler_files, fn {namespace, filename} ->
      path = Path.join(@base_path, filename)
      IO.puts("Importing #{namespace} from #{path}...")

      result = Nova.MCP.Tools.call_tool("nova_import_file", %{
        "path" => path,
        "namespace" => namespace
      }, svc)

      text = result["content"] |> hd |> Map.get("text")
      is_error = result["isError"]

      if is_error do
        IO.puts("  ERROR: #{text}")
      else
        IO.puts("  OK: #{text}")
      end

      {namespace, is_error, text}
    end)

    IO.puts("\n=== Import Summary ===")
    errors = Enum.filter(results, fn {_, is_error, _} -> is_error end)
    success = Enum.filter(results, fn {_, is_error, _} -> not is_error end)

    IO.puts("Success: #{length(success)}")
    IO.puts("Errors: #{length(errors)}")

    if length(errors) > 0 do
      IO.puts("\nFailed imports:")
      Enum.each(errors, fn {ns, _, text} ->
        IO.puts("  - #{ns}: #{String.slice(text, 0, 100)}")
      end)
    end

    # Try to validate each namespace
    IO.puts("\n=== Validation ===")
    Enum.each(success, fn {namespace, _, _} ->
      IO.puts("\nValidating #{namespace}...")
      result = Nova.MCP.Tools.call_tool("nova_validate", %{"namespace" => namespace}, svc)
      text = result["content"] |> hd |> Map.get("text")
      is_error = result["isError"]

      if is_error do
        IO.puts("  ERROR: #{String.slice(text, 0, 200)}")
      else
        IO.puts("  OK: #{text}")
      end
    end)

    IO.puts("\n=== Done ===")
  end
end

CompilerImporter.run()
