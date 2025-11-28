# Test various guard and RHS combinations

test_cases = [
  # Simple guard, simple RHS (known working)
  {"simple guard", """
module Test where
foo (PatVar name) n | name == "_" = 1
foo (PatVar name) n = 2
"""},

  # Qualified function in guard
  {"qualified func in guard", """
module Test where
foo (PatVar name) n | String.take 1 name == "_" = 1
foo (PatVar name) n = 2
"""},

  # Record literal RHS
  {"record RHS", """
module Test where
foo x = { str: "a", counter: 1 }
"""},

  # Guard with record RHS
  {"guard + record RHS", """
module Test where
foo (PatVar name) n | name == "_" = { str: "a", counter: 1 }
foo (PatVar name) n = { str: "b", counter: 2 }
"""},

  # String concat in record
  {"string concat in record", """
module Test where
foo n = { str: "_W" <> show n, counter: n + 1 }
"""},

  # Full complex case
  {"full complex", """
module Test where
foo (PatVar name) n | String.take 1 name == "_" = { str: "_W" <> show n, counter: n + 1 }
foo (PatVar name) n = { str: name, counter: n }
"""},
]

for {label, code} <- test_cases do
  tokens = Nova.Compiler.Tokenizer.tokenize(code)
  result = case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} -> "FAIL: #{inspect(err) |> String.slice(0, 60)}"
    {:right, {:tuple, mod, rest}} -> "OK #{length(mod.declarations)} decls"
  end
  IO.puts("#{label}: #{result}")
end
