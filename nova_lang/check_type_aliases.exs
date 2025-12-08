# Check type alias parsing in Cst
File.cd!("..")

source = File.read!("./src/Nova/Compiler/Cst.purs")
result = Nova.Compiler.CstPipeline.parse_module_cst(source)
case result do
  {:right, mod} ->
    # Find type alias declarations
    type_aliases = Enum.filter(Enum.to_list(mod.declarations), fn
      {:decl_type_alias, _} -> true
      _ -> false
    end)
    IO.puts("Found #{length(type_aliases)} type aliases:")
    Enum.each(type_aliases, fn {:decl_type_alias, ta} ->
      IO.puts("  - #{ta.name}")
    end)
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
