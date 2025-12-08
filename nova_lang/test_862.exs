source = File.read!("purs_tests/862.purs")
{:ok, code} = Nova.compile(source)
IO.puts(code)
