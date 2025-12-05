# Test CST parsing only, before conversion to AST
source = File.read!("../src/Nova/Compiler/Types.purs")

# Just use the lexer and parser
tokens = Nova.Compiler.CstLexer.lex_module(source)
IO.puts("Lexed #{length(tokens)} tokens")

# Run layout
laid_out = Nova.Compiler.CstLayout.layout(tokens)
IO.puts("Layout produced #{length(laid_out)} tokens")

# Parse
result = Nova.Compiler.CstParser.run_parser(Nova.Compiler.CstParser.parse_module(), laid_out)

case result do
  {:right, {module, remaining}} ->
    IO.puts("Parsed CST with #{length(module.declarations)} declarations, #{length(remaining)} remaining tokens")
    
    # Show first 25 decl types
    module.declarations
    |> Enum.take(25)
    |> Enum.with_index()
    |> Enum.each(fn {decl, i} ->
      tag = elem(decl, 0)
      IO.puts("  #{i}: #{tag}")
    end)
    
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
