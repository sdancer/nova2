defmodule Nova.Compiler.CstLayout do
  # import Prelude

  # import Data.List

  # import Data.Maybe

  # import Data.Tuple

  # import Data.Foldable

  # import Nova.Compiler.Cst

  # @type layout_stack :: list()((tuple()(source_pos())(layout_delim())))

  # Data type: LayoutDelim
  def lyt_root(), do: :lyt_root
  def lyt_top_decl(), do: :lyt_top_decl
  def lyt_top_decl_head(), do: :lyt_top_decl_head
  def lyt_decl_guard(), do: :lyt_decl_guard
  def lyt_case(), do: :lyt_case
  def lyt_case_binders(), do: :lyt_case_binders
  def lyt_case_guard(), do: :lyt_case_guard
  def lyt_lambda_binders(), do: :lyt_lambda_binders
  def lyt_paren(), do: :lyt_paren
  def lyt_brace(), do: :lyt_brace
  def lyt_square(), do: :lyt_square
  def lyt_if(), do: :lyt_if
  def lyt_then(), do: :lyt_then
  def lyt_property(), do: :lyt_property
  def lyt_forall(), do: :lyt_forall
  def lyt_tick(), do: :lyt_tick
  def lyt_let(), do: :lyt_let
  def lyt_let_stmt(), do: :lyt_let_stmt
  def lyt_where(), do: :lyt_where
  def lyt_of(), do: :lyt_of
  def lyt_do(), do: :lyt_do
  def lyt_ado(), do: :lyt_ado

  # derive instance Eq layout_delim()

  # derive instance Ord layout_delim()



  def is_indented() do
    fn $lamcase -> case $lamcase do
      :lyt_let -> true
      :lyt_let_stmt -> true
      :lyt_where -> true
      :lyt_of -> true
      :lyt_do -> true
      :lyt_ado -> true
      _ -> false
    end end
  end



  def current_indent_go() do
    fn $lamcase -> case $lamcase do
      [{:tuple, pos, lyt} | _stk] ->
        if is_indented(lyt) do
          {:just, pos}
        end
      _ -> :nothing
    end end
  end



  def current_indent() do
    current_indent_go()
  end



  def is_top_decl(tok_pos, stack) do
    case stack do
      [{:tuple, lyt_pos, lyt1} | rest] -> if (lyt1 == (&lyt_where/1)) do
          case rest do
            [{:tuple, _, lyt2} | :nil_] -> (((lyt2 == (&lyt_root/1)) and tok_pos.column) == lyt_pos.column)
            _ -> false
          end
        else
          false
        end
      _ -> false
    end
  end



  def lyt_token(pos, value) do
    %{range: %{start: pos, end_: pos}, leading_comments: [], trailing_comments: [], value: value}
  end



  def insert_layout(src, next_pos, stack) do
    insert.(({:tuple, stack, []}))
  end
end
