
modules = [
  {"../src/Nova/Compiler/Ast.purs", "Ast"},
  {"../src/Nova/Compiler/Types.purs", "Types"},
  {"../src/Nova/Compiler/Tokenizer.purs", "Tokenizer"},
  {"../src/Nova/Compiler/Unify.purs", "Unify"},
  {"../src/Nova/Compiler/TypeChecker.purs", "TypeChecker"},
  {"../src/Nova/Compiler/CodeGen.purs", "CodeGen"},
  {"../src/Nova/Compiler/Parser.purs", "Parser"},
  {"../src/Nova/Compiler/Dependencies.purs", "Dependencies"},
  {"../src/Nova/Compiler/CodeGenWasm.purs", "CodeGenWasm"}
]

for {path, name} <- modules do
  source = File.read!(path)
  tokens = Nova.Compiler.Tokenizer.tokenize(source)
  case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} ->
      IO.puts("ERROR #{name}: #{inspect(err)}")
    {:right, {:tuple, mod, _}} ->
      wat = Nova.Compiler.CodeGenWasm.gen_module(mod)
      out_path = "/tmp/nova_wasm_stage2/#{name}.wat"
      File.write!(out_path, wat)
      lines = length(String.split(wat, "\n"))
      IO.puts("OK #{name}: #{lines} lines")
  end
end
