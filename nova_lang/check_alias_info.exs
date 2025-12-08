# Check type alias info for SourcePos from Cst
File.cd!("..")

source = File.read!("./src/Nova/Compiler/Cst.purs")
result = Nova.Compiler.CstPipeline.parse_module_cst(source)
case result do
  {:right, mod} ->
    decls = Enum.to_list(mod.declarations)
    exports = Nova.Compiler.TypeChecker.extract_exports(decls)

    IO.puts("Type aliases in exports:")
    Enum.each(exports.type_aliases, fn {name, info} ->
      IO.puts("  #{name}:")
      IO.puts("    params: #{inspect(info.params)}")
      IO.puts("    body: #{inspect(info.body)}")
    end)
  {:left, err} ->
    IO.puts("Parse error: #{inspect(err)}")
end
