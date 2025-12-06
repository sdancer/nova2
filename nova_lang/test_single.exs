# Test a single purs file
file = System.argv() |> List.first() || "purs_tests/1110.purs"

code = File.read!(file)

case Nova.Compiler.CstPipeline.parse_module_cst(code) do
  {:right, ast} ->
    result = Nova.Compiler.CodeGen.gen_module(ast)
    IO.puts(result)
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
