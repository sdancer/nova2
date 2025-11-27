Code.compile_file("nova_runtime.ex")
Code.compile_file("Ast.ex")
Code.compile_file("Tokenizer.ex")
Code.compile_file("Parser.ex")
Code.compile_file("Types.ex")
Code.compile_file("Unify.ex")
Code.compile_file("TypeChecker.ex")

source = File.read!("../src/Nova/Compiler/Parser.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)
{:right, {:tuple, m, _}} = Nova.Compiler.Parser.parse_module(tokens)

# Find the function declarations and type aliases
type_aliases = for {:decl_type_alias, ta} <- m.declarations, do: ta.name
IO.puts("Type aliases: #{Enum.take(type_aliases, 10) |> Enum.join(", ")}...")

# Check type checking step by step
env = Nova.Compiler.Types.empty_env()
env1 = Nova.Compiler.TypeChecker.process_non_functions(env, m.declarations)
IO.puts("\nAfter process_non_functions, checking if FunctionDeclaration is in env...")
case Nova.Compiler.Types.lookup_env(env1, "FunctionDeclaration") do
  {:just, scheme} -> IO.puts("FunctionDeclaration found: #{inspect(scheme, limit: 5)}")
  :nothing -> IO.puts("FunctionDeclaration NOT found")
end

env2 = Nova.Compiler.TypeChecker.add_function_placeholders(env1, m.declarations)
IO.puts("\nAfter add_function_placeholders...")

# Now try checking functions one by one
merged = Nova.Compiler.TypeChecker.merge_multi_clause_functions(m.declarations)
funcs = for {:decl_function, f} <- merged, do: f.name

IO.puts("\nChecking functions one by one...")
Enum.reduce_while(funcs, env2, fn name, env ->
  func = Enum.find(merged, fn 
    {:decl_function, f} -> f.name == name
    _ -> false
  end)
  
  case func do
    {:decl_function, f} ->
      case Nova.Compiler.TypeChecker.check_function(env, f) do
        {:right, res} -> 
          {:cont, res.env}
        {:left, err} ->
          IO.puts("FAILED at function: #{name}")
          IO.puts("Error: #{inspect(err, limit: 5)}")
          {:halt, env}
      end
    _ -> {:cont, env}
  end
end)
