src = File.read!("../src/Nova/Compiler/Unify.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(src)
{:right, {:tuple, mod, _}} = Nova.Compiler.Parser.parse_module(tokens)

# Load Types.purs as dependency
types_src = File.read!("../src/Nova/Compiler/Types.purs")
types_tokens = Nova.Compiler.Tokenizer.tokenize(types_src)
{:right, {:tuple, types_mod, _}} = Nova.Compiler.Parser.parse_module(types_tokens)

all_decls = types_mod.declarations ++ mod.declarations
env = Nova.Compiler.Types.empty_env()
result = Nova.Compiler.TypeChecker.check_module(env, all_decls)
IO.inspect(result, limit: :infinity)
