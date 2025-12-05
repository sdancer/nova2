# Test CST parsing before conversion to AST
source = File.read!("../src/Nova/Compiler/Types.purs")

# Get raw CST module (before AST conversion)
cst = Nova.Compiler.CstPipeline.parse_module_to_cst(source)

case cst do
  {:right, cst_module} ->
    decls = cst_module.body.decls
    IO.puts("Parsed raw CST module with #{length(decls)} declarations")
    
    # Show first 40 declarations
    decls
    |> Enum.take(40)
    |> Enum.with_index()
    |> Enum.each(fn {decl, i} ->
      tag = elem(decl, 0)
      IO.puts("  #{i}: #{tag}")
    end)
    
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
