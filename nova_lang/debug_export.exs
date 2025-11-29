# Debug what gets exported from Types

# Parse Types.purs
src = File.read!("../src/Nova/Compiler/Types.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(src)
{:right, {:tuple, mod, _}} = Nova.Compiler.Parser.parse_module(tokens)

# Extract exports (this happens BEFORE type checking - it's just from declarations!)
exports = Nova.Compiler.TypeChecker.extract_exports(mod.declarations)

IO.puts("=== Constructors exported from Types ===")
for {name, scheme} <- exports.constructors do
  IO.puts("\n#{name}:")
  IO.inspect(scheme, limit: :infinity)
end
