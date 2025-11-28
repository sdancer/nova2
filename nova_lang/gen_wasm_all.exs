modules = [
  {"Ast", "../src/Nova/Compiler/Ast.purs"},
  {"Types", "../src/Nova/Compiler/Types.purs"},
  {"Tokenizer", "../src/Nova/Compiler/Tokenizer.purs"},
  {"Unify", "../src/Nova/Compiler/Unify.purs"},
  {"TypeChecker", "../src/Nova/Compiler/TypeChecker.purs"},
  {"CodeGen", "../src/Nova/Compiler/CodeGen.purs"},
  {"Parser", "../src/Nova/Compiler/Parser.purs"},
  {"Dependencies", "../src/Nova/Compiler/Dependencies.purs"},
  {"CodeGenWasm", "../src/Nova/Compiler/CodeGenWasm.purs"}
]

File.mkdir_p!("/tmp/wasm_beam")

for {name, path} <- modules do
  source = File.read!(path)
  tokens = Nova.Compiler.Tokenizer.tokenize(source)
  case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} ->
      IO.puts("[BEAM] #{name}: ERROR #{inspect(err)}")
    {:right, {:tuple, mod, _}} ->
      wat = Nova.Compiler.CodeGenWasm.gen_module(mod)
      File.write!("/tmp/wasm_beam/#{name}.wat", wat)
      lines = length(String.split(wat, "\n"))
      IO.puts("[BEAM] #{name}: #{lines} lines")
  end
end
