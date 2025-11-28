source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Test lines 1-100
code_100 = Enum.take(lines, 100) |> Enum.join("\n")
tokens = Nova.Compiler.Tokenizer.tokenize(code_100)
IO.puts("Lines 1-100: #{length(tokens)} tokens")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("FAIL: #{inspect(err)}")
  {:right, {:tuple, mod, rest}} ->
    IO.puts("OK: #{length(mod.declarations)} declarations")
    IO.puts("Remaining tokens: #{length(rest)}")

    # Show declaration names
    for d <- mod.declarations do
      case d do
        {:decl_function, f} -> IO.puts("  Function: #{f.name}")
        {:decl_type_sig, sig} -> IO.puts("  TypeSig: #{sig.name}")
        {:decl_data_type, dt} -> IO.puts("  DataType: #{dt.name}")
        {:decl_import, imp} -> IO.puts("  Import: #{imp.module_name}")
        _ -> IO.puts("  Other: #{inspect(d)}")
      end
    end

    if length(rest) > 0 do
      IO.puts("\nRemaining tokens:")
      for t <- Enum.take(rest, 10) do
        IO.puts("  #{t.token_type}: '#{String.slice(to_string(t.value), 0, 30)}' line #{t.line}")
      end
    end
end
