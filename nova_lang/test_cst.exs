# Test CST parsing of Types.purs
source = File.read!("../src/Nova/Compiler/Types.purs")
cst = Nova.Compiler.CstPipeline.parse_module_cst(source)

case cst do
  {:right, ast} ->
    IO.puts("Successfully parsed AST with #{length(ast.declarations)} declarations")
    
    # Show first few declarations
    ast.declarations
    |> Enum.take(20)
    |> Enum.with_index()
    |> Enum.each(fn {decl, i} ->
      name = case decl do
        {:decl_type_alias, ta} -> "TypeAlias: #{ta.name}"
        {:decl_function, f} -> "Function: #{f.name}"
        {:decl_data, d} -> "Data: #{d.name}"
        {:decl_newtype, n} -> "Newtype: #{n.name}"
        other -> "Other: #{inspect(elem(other, 0))}"
      end
      IO.puts("  #{i}: #{name}")
    end)
    
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
