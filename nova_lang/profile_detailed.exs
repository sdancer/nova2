# Profile compilation in detail
base_path = "/home/sdancer/nova2/src/Nova/Compiler"

# Unify depends on Types
main_source = File.read!("#{base_path}/Unify.purs")
dep_source = File.read!("#{base_path}/Types.purs")

IO.puts("=== Profiling Unify with Types dependency ===")
IO.puts("Main: #{String.length(main_source)} chars")
IO.puts("Dep: #{String.length(dep_source)} chars\n")

# Step 1: Tokenize dep
{t1, dep_tokens} = :timer.tc(fn -> Nova.Compiler.Tokenizer.tokenize(dep_source) end)
IO.puts("1. Tokenize Types: #{t1 / 1000}ms (#{length(dep_tokens)} tokens)")

# Step 2: Parse dep
{t2, dep_result} = :timer.tc(fn -> Nova.Compiler.Parser.parse_module(dep_tokens) end)
{:right, {:tuple, dep_mod, _}} = dep_result
IO.puts("2. Parse Types: #{t2 / 1000}ms (#{length(dep_mod.declarations)} decls)")

# Step 3: Tokenize main
{t3, main_tokens} = :timer.tc(fn -> Nova.Compiler.Tokenizer.tokenize(main_source) end)
IO.puts("3. Tokenize Unify: #{t3 / 1000}ms (#{length(main_tokens)} tokens)")

# Step 4: Parse main
{t4, main_result} = :timer.tc(fn -> Nova.Compiler.Parser.parse_module(main_tokens) end)
{:right, {:tuple, main_mod, _}} = main_result
IO.puts("4. Parse Unify: #{t4 / 1000}ms (#{length(main_mod.declarations)} decls)")

# Step 5: Type check
all_decls = dep_mod.declarations ++ main_mod.declarations
{t5, tc_result} = :timer.tc(fn -> 
  Nova.Compiler.TypeChecker.check_module(Nova.Compiler.Types.empty_env(), all_decls)
end)
IO.puts("5. Type check: #{t5 / 1000}ms")

# Step 6: Code gen
{t6, _code} = :timer.tc(fn -> Nova.Compiler.CodeGen.gen_module(main_mod) end)
IO.puts("6. CodeGen: #{t6 / 1000}ms")

IO.puts("\nTotal: #{(t1 + t2 + t3 + t4 + t5 + t6) / 1000}ms")
