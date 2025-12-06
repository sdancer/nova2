# Debug test for 1185.purs
code = File.read!("purs_tests/1185.purs")
IO.puts("=== Source ===")
IO.puts(code)

IO.puts("\n=== Parsing ===")
case Nova.Compiler.CstPipeline.parse_module_cst(code) do
  {:right, ast} ->
    IO.puts("Parse success")

    IO.puts("\n=== Generating Elixir ===")
    elixir_code = Nova.Compiler.CodeGen.gen_module(ast)
    IO.puts(elixir_code)

    IO.puts("\n=== Loading ===")
    try do
      Code.compile_string(elixir_code)
      IO.puts("SUCCESS!")
    rescue
      e ->
        IO.puts("LOAD ERROR: #{Exception.message(e)}")
        IO.inspect(__STACKTRACE__ |> Enum.take(5), label: "Stack")
    end
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
