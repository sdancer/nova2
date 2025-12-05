# Test CST parsing before conversion to AST
source = File.read!("../src/Nova/Compiler/Types.purs")

# Get raw CST module (before AST conversion)
cst = Nova.Compiler.CstPipeline.parse_module_to_cst(source)

case cst do
  {:right, cst_module} ->
    # Explore the structure
    IO.puts("Keys: #{inspect(Map.keys(cst_module))}")
    if Map.has_key?(cst_module, :body) do
      IO.puts("Body keys: #{inspect(Map.keys(cst_module.body))}")
    end
    if Map.has_key?(cst_module, :declarations) do
      IO.puts("Declarations count: #{length(cst_module.declarations)}")
    end
    
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
