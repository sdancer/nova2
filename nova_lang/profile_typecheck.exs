# Profile type checking - simple timing
source = File.read!("/home/sdancer/nova2/src/Nova/Compiler/Unify.purs")
IO.puts("Profiling Unify.purs (#{String.length(source)} chars)")

# Parse
tokens = Nova.Compiler.Tokenizer.tokenize(source)
result = Nova.Compiler.Parser.parse_module(tokens)

mod = case result do
  {:right, {:tuple, m, _}} -> m
  {:right, m} when is_map(m) -> m
end

IO.puts("Declarations: #{length(mod.declarations)}")

# Time type checking
{time, tc_result} = :timer.tc(fn ->
  Nova.Compiler.TypeChecker.check_module(Nova.Compiler.Types.empty_env(), mod.declarations)
end)

IO.puts("Type check time: #{time / 1000}ms")

case tc_result do
  {:right, typed_env} -> 
    IO.puts("Type check: OK")
    # Time code generation
    {cg_time, _code} = :timer.tc(fn ->
      Nova.Compiler.CodeGen.gen_module(mod)
    end)
    IO.puts("CodeGen time: #{cg_time / 1000}ms")
  {:left, err} -> 
    IO.puts("Type check error: #{inspect(err)}")
end
