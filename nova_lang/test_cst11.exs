# Test simple case expression
code = """
module Test where

foo x =
  case x of
    Nothing -> 1
    Just y -> y

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
