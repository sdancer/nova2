source = File.read!("../src/Nova/Compiler/CodeGenCoreErlang.purs")
lines = String.split(source, "\n")

# Lines 1-150
code = Enum.take(lines, 150) |> Enum.join("\n")
tokens = Nova.Compiler.Tokenizer.tokenize(code)
IO.puts("Lines 1-150: #{length(tokens)} tokens")

# Parse and examine failure
case Nova.Compiler.Parser.parse_module(tokens) do
  {:left, err} ->
    IO.puts("Error: #{inspect(err)}")

    # Try to find where it fails by examining declarations
    # If we can get intermediate state...

    # Find the last token that gives a valid parse
    defmodule Search do
      def find(toks, lo, hi, last) when lo >= hi, do: last
      def find(toks, lo, hi, last) do
        mid = div(lo + hi, 2)
        case Nova.Compiler.Parser.parse_module(Enum.take(toks, mid)) do
          {:right, {:tuple, mod, rest}} when length(rest) == 0 ->
            find(toks, mid + 1, hi, {mid, length(mod.declarations)})
          _ ->
            find(toks, lo, mid, last)
        end
      end
    end

    {last_tok, decls} = Search.find(tokens, 0, length(tokens), {0, 0})
    IO.puts("Last clean parse: #{last_tok} tokens → #{decls} declarations")

    # Show context around last_tok
    IO.puts("\nTokens around #{last_tok}:")
    for i <- [last_tok-3, last_tok-2, last_tok-1, last_tok, last_tok+1, last_tok+2, last_tok+3, last_tok+4, last_tok+5] do
      if i >= 0 && i < length(tokens) do
        t = Enum.at(tokens, i)
        IO.puts("  #{i}: #{t.token_type} '#{String.slice(to_string(t.value), 0, 30)}' line #{t.line}")
      end
    end

  {:right, {:tuple, mod, rest}} ->
    IO.puts("OK: #{length(mod.declarations)} decls, #{length(rest)} rest")
end
