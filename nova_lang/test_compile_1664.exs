code = File.read!("purs_tests/1664.purs")
result = Nova.compile(code)
IO.inspect(result, limit: :infinity)
