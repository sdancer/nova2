# Test compiling Regenerate.purs using the Elixir compiler
source = File.read!("../src/Nova/Compiler/Regenerate.purs")

# Parse with CST pipeline
case Nova.Compiler.CstPipeline.parse_module_cst(source) do
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
  {:right, ast} ->
    IO.puts("Parsed successfully!")
    IO.puts("Module name: #{ast.name}")
    IO.puts("Declaration count: #{length(ast.declarations)}")

    # Try code gen directly (skip type checking since that requires imports)
    IO.puts("Attempting code generation...")
    try do
      code = Nova.Compiler.CodeGen.gen_module().(ast)
      IO.puts("Code generation succeeded!")
      IO.puts("Generated #{String.length(code)} chars")

      # Show first 500 chars
      IO.puts("\n=== First 500 chars ===")
      IO.puts(String.slice(code, 0, 500))
    rescue
      e ->
        IO.puts("Code generation error: #{inspect(e)}")
        IO.puts(Exception.format_stacktrace(__STACKTRACE__))
    end
end
