source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Test lines 1-100
code_100 = Enum.take(lines, 100) |> Enum.join("\n")
tokens_100 = Nova.Compiler.Tokenizer.tokenize(code_100)
IO.puts("Lines 1-100: #{length(tokens_100)} tokens")

case Nova.Compiler.Parser.parse_module(tokens_100) do
  {:left, err} -> IO.puts("Lines 1-100 FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} -> IO.puts("Lines 1-100 OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end

# Test lines 1-150
code_150 = Enum.take(lines, 150) |> Enum.join("\n")
tokens_150 = Nova.Compiler.Tokenizer.tokenize(code_150)
IO.puts("\nLines 1-150: #{length(tokens_150)} tokens")

case Nova.Compiler.Parser.parse_module(tokens_150) do
  {:left, err} -> IO.puts("Lines 1-150 FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} -> IO.puts("Lines 1-150 OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end

# Try parsing first 888 tokens from 150-line file
IO.puts("\nFirst 888 tokens from lines 1-150:")
tokens_888 = Enum.take(tokens_150, 888)
case Nova.Compiler.Parser.parse_module(tokens_888) do
  {:left, err} -> IO.puts("888 tokens FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} -> IO.puts("888 tokens OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end
