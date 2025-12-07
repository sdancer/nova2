source = File.read!("purs_tests/1807.purs")
{:right, ast} = Nova.Compiler.CstPipeline.parse_module_cst(source)
code = Nova.Compiler.CodeGen.gen_module().(ast)

IO.puts("Generated code:")
IO.puts(code)
