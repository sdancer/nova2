# Debug constructor types

# Parse Types.purs
src = File.read!("../src/Nova/Compiler/Types.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(src)
{:right, {:tuple, mod, _}} = Nova.Compiler.Parser.parse_module(tokens)

# Type check it standalone
env = Nova.Compiler.Types.empty_env()
result = Nova.Compiler.TypeChecker.check_module(env, mod.declarations)

case result do
  {:right, checked_env} ->
    IO.puts("Type checked Types.purs successfully")
    # Look up the TyVar constructor
    case Nova.Compiler.Types.lookup_env(checked_env, "TyVar") do
      {:just, scheme} ->
        IO.puts("\nTyVar constructor scheme:")
        IO.inspect(scheme, limit: :infinity)
      :nothing ->
        IO.puts("\nTyVar not found in environment")
    end
  {:left, err} ->
    IO.puts("Type check failed: #{inspect(err)}")
end
