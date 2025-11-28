source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")
code_109 = Enum.take(lines, 109) |> Enum.join("\n")

tokens = Nova.Compiler.Tokenizer.tokenize(code_109)
IO.puts("Tokens: #{length(tokens)}")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("FAIL: #{inspect(err)}")
    # Show what we parsed so far
    for n <- [105, 106, 107, 108, 109] do
      t = Enum.at(tokens, n)
      if t, do: IO.puts("Token #{n}: #{inspect(t)}")
    end
  {:right, {:tuple, mod, rest}} ->
    IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end

# Also try with JS parser for comparison
IO.puts("\n--- Testing with JS parser ---")
result = System.cmd("node", ["-e", ~s|
const fs = require('fs');
const {tokenize} = require('../output/Nova.Compiler.Tokenizer/index.js');
const {parse_module} = require('../output/Nova.Compiler.Parser/index.js');
const source = fs.readFileSync('../src/Nova/Compiler/CodeGenCoreErlang.purs', 'utf8');
const lines = source.split('\\n').slice(0, 109).join('\\n');
const tokens = tokenize(lines);
console.log('JS Tokens:', tokens.length);
const result = parse_module(tokens);
if (result.value0) {
  console.log('JS FAIL:', result.value0);
} else {
  console.log('JS OK:', result.value0.value0.declarations.length, 'decls');
}
|], stderr_to_stdout: true)
IO.puts(elem(result, 0))
