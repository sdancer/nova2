lines_to_try = [130, 135, 140, 145, 150, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200]

for n <- lines_to_try do
  source = "../src/Nova/Compiler/CodeGenCoreErlang.purs"
           |> File.read!()
           |> String.split("\n")
           |> Enum.take(n)
           |> Enum.join("\n")

  tokens = Nova.Compiler.Tokenizer.tokenize(source)

  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{inspect(err) |> String.slice(0, 30)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end

  IO.puts("Lines #{n}: #{result}")
end
