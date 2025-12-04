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
        tokens = lex_module.(source)
  case run_parser.(parse_module).(tokens) do
    {:left, err} -> {:left, err}
    {:right, cst_result} ->
            cst_mod = Nova.Runtime.fst(cst_result)
      convert_module.(cst_mod)
  end
  end



  def parse_module_to_cst(source) do
        tokens = lex_module.(source)
  case run_parser.(parse_module).(tokens) do
    {:left, err} -> {:left, err}
    {:right, cst_result} ->
      Nova.Runtime.pure((Nova.Runtime.fst(cst_result)))
  end
  end



  def lex_source() do
    lex_module
  end
end
