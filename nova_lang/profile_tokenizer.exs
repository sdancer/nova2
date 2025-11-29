# Profile the tokenizer to find bottlenecks
source = File.read!("/home/sdancer/nova2/src/Nova/Compiler/Ast.purs")
IO.puts("Source: #{String.length(source)} chars")

# Profile with eprof
:eprof.start()
:eprof.start_profiling([self()])

tokens = Nova.Compiler.Tokenizer.tokenize(source)

:eprof.stop_profiling()
:eprof.analyze(:total)

IO.puts("\nTokens: #{length(tokens)}")
