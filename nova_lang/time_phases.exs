# Time each phase: tokenize, parse, codegen, elixirc

src = File.read!("../src/Nova/Compiler/Parser.purs")
IO.puts("Source: #{byte_size(src)} bytes")

# Tokenize with fast tokenizer
{tok_us, tokens} = :timer.tc(fn -> Nova.FastTokenizer.tokenize(src) end)
IO.puts("\n1. Tokenize (fast): #{Float.round(tok_us/1000, 2)}ms (#{length(tokens)} tokens)")

# Parse
{parse_us, result} = :timer.tc(fn -> Nova.Compiler.Parser.parse_module(tokens) end)
{:right, {:tuple, mod, _}} = result
IO.puts("2. Parse: #{Float.round(parse_us/1000, 2)}ms")

# CodeGen
{codegen_us, code} = :timer.tc(fn -> Nova.Compiler.CodeGen.gen_module(mod) end)
IO.puts("3. CodeGen: #{Float.round(codegen_us/1000, 2)}ms (#{String.length(code)} chars)")

# Write to temp file
tmp_file = "/tmp/test_compile.ex"
File.write!(tmp_file, code)

# Elixir compile
{elixirc_us, _} = :timer.tc(fn ->
  System.cmd("elixirc", [tmp_file], stderr_to_stdout: true)
end)
IO.puts("4. elixirc: #{Float.round(elixirc_us/1000, 2)}ms")

total = tok_us + parse_us + codegen_us + elixirc_us
IO.puts("\nTotal: #{Float.round(total/1000, 2)}ms")
IO.puts("\nBreakdown:")
IO.puts("  Tokenize: #{Float.round(tok_us/total*100, 1)}%")
IO.puts("  Parse:    #{Float.round(parse_us/total*100, 1)}%")
IO.puts("  CodeGen:  #{Float.round(codegen_us/total*100, 1)}%")
IO.puts("  elixirc:  #{Float.round(elixirc_us/total*100, 1)}%")
