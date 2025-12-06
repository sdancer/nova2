code = File.read!("purs_tests/1110.purs")
result = Nova.compile(code)
IO.inspect(result, limit: :infinity)
