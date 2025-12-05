defmodule Nova.Compiler.CstPipeline do
  # import Prelude

  # import Data.List

  # import Data.Either

  # import Data.Tuple

  # import Data.Void

  # import Nova.Compiler.Cst

  # import Nova.Compiler.CstLexer

  # import Nova.Compiler.CstParser

  # import Nova.Compiler.CstToAst

  # import Nova.Compiler.Ast



  def parse_module_cst(source) do
        tokens = Nova.Compiler.CstLexer.lex_module(source)
  Nova.Runtime.bind((Nova.Compiler.CstParser.run_parser(Nova.Compiler.CstParser.parse_module)).(tokens), fn cst_result ->
        cst_mod = Nova.Runtime.fst(cst_result)
    Nova.Compiler.CstToAst.convert_module(cst_mod)
  end)
  end



  def parse_module_to_cst(source) do
        tokens = Nova.Compiler.CstLexer.lex_module(source)
  Nova.Runtime.bind((Nova.Compiler.CstParser.run_parser(Nova.Compiler.CstParser.parse_module)).(tokens), fn cst_result ->
    Nova.Runtime.pure((Nova.Runtime.fst(cst_result)))
  end)
  end



  def lex_source() do
    Nova.Compiler.CstLexer.lex_module
  end
end
