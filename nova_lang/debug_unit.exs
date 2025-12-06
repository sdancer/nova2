# Debug Unit test patching

source = File.read!("purs_tests/Unit.purs")
{:right, ast} = Nova.Compiler.CstPipeline.parse_module_cst(source)
code = Nova.Compiler.CodeGen.gen_module().(ast)
IO.puts("=== Generated ===")
IO.puts(code)

# Apply patches (same as run_purs_passing_tests.exs)
patched = code
  |> String.replace(~r/(?<!Runtime\.)log_show\.\(/, "Nova.Runtime.log_show(")
  |> String.replace(~r/(?<!Runtime\.)(?<!&)log\.\(/, "Nova.Runtime.log(")

IO.puts("")
IO.puts("=== Patched ===")
IO.puts(patched)

# Try to compile
IO.puts("")
IO.puts("=== Compiling ===")
try do
  Code.compile_string(patched)
  IO.puts("SUCCESS!")
rescue
  e -> IO.puts("ERROR: #{Exception.message(e)}")
end
