# Debug test for Comparisons.purs
code = File.read!("purs_tests/Comparisons.purs")
IO.puts("=== Source ===")
IO.puts(code)

IO.puts("\n=== Generating Elixir ===")
case Nova.Compiler.CstPipeline.parse_module_cst(code) do
  {:right, ast} ->
    elixir_code = Nova.Compiler.CodeGen.gen_module(ast)
    IO.puts(elixir_code)

  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
