source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Test lines 1-150
code_150 = Enum.take(lines, 150) |> Enum.join("\n")
tokens = Nova.Compiler.Tokenizer.tokenize(code_150)
IO.puts("Lines 1-150: #{length(tokens)} tokens")

case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("FAIL: #{inspect(err)}")

    # Try to see how many declarations we can parse
    # by binary searching on token count
    defmodule BS do
      def find(toks, lo, hi, last) when lo >= hi, do: last
      def find(toks, lo, hi, last) do
        mid = div(lo + hi, 2)
        case Nova.Compiler.Parser.parse_module(Enum.take(toks, mid)) do
          {:right, {:tuple, mod, _}} when length(mod.declarations) > 0 -> find(toks, mid + 1, hi, mid)
          _ -> find(toks, lo, mid, last)
        end
      end
    end

    lg = BS.find(tokens, 0, length(tokens), 0)
    IO.puts("Last good token count: #{lg}")

    # Parse with last good count and show remaining
    case Nova.Compiler.Parser.parse_module(Enum.take(tokens, lg)) do
      {:right, {:tuple, mod, rest}} ->
        IO.puts("Decls at #{lg} tokens: #{length(mod.declarations)}")
        IO.puts("Rest tokens: #{length(rest)}")

        # Show the next few tokens after last good
        IO.puts("Tokens after good point:")
        for i <- [lg, lg+1, lg+2, lg+3, lg+4] do
          t = Enum.at(tokens, i)
          if t do
            IO.puts("  Token #{i}: #{t.token_type} '#{String.slice(to_string(t.value), 0, 30)}' line #{t.line}")
          end
        end
      _ -> :ok
    end

  {:right, {:tuple, mod, rest}} ->
    IO.puts("OK: #{length(mod.declarations)} declarations")
    IO.puts("Remaining tokens: #{length(rest)}")
end
