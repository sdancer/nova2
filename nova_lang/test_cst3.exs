# Test CST parsing using the pipeline
source = File.read!("../src/Nova/Compiler/Types.purs")

# Get CST module
cst = Nova.Compiler.CstPipeline.parse_source(source)

case cst do
  {:right, cst_module} ->
    IO.puts("Parsed CST module with #{length(cst_module.declarations)} declarations")
    
    # Show declarations
    cst_module.declarations
    |> Enum.take(25)
    |> Enum.with_index()
    |> Enum.each(fn {decl, i} ->
      tag = elem(decl, 0)
      IO.puts("  #{i}: #{tag}")
    end)
    
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
