# Test the atom function pattern

test1 = """
module Test where
atom s = "'" <> escapeAtom s <> "'"
  where
  escapeAtom str =
    let s1 = replaceAll str
        s2 = replaceAll s1
    in s2
"""

test2 = """
module Test where
atom :: String -> String
atom s = "'" <> escapeAtom s <> "'"
  where
  escapeAtom str =
    let s1 = replaceAll str
        s2 = replaceAll s1
    in s2
"""

test3 = """
module Test where
atom :: String -> String
atom s = "'" <> escapeAtom s <> "'"
  where
  escapeAtom str =
    let s1 = replaceAll str
    in s1
"""

for {label, code} <- [
  {"no type sig", test1},
  {"with type sig", test2},
  {"simple let", test3}
] do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{String.slice(inspect(err), 0, 50)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls, #{length(rest)} rest"
  end
  IO.puts("#{label}: #{result}")
end
