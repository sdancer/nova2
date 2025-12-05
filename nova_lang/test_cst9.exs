# Test minimal case to find what syntax is breaking
code = """
module Test where

foo = String.lastIndexOf (String.Pattern ".") name

bar = 1
"""

cst = Nova.Compiler.CstPipeline.parse_module_to_cst(code)

case cst do
  {:right, cst_module} ->
    decls = cst_module.body.decls
    IO.puts("Parsed #{length(decls)} declarations:")
    Enum.each(decls, fn d -> IO.puts("  #{elem(d, 0)}") end)
    
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
