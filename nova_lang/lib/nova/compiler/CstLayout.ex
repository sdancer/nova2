defmodule Nova.Compiler.CstLayout do
  # import Prelude

  # import Data.List

  # import Data.Maybe

  # import Data.Tuple

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



  def root_layout_delim() do
    :lyt_root
  end



  def is_indented(lyt) do
    case lyt do
      :lyt_let -> true
      :lyt_let_stmt -> true
      :lyt_where -> true
      :lyt_of -> true
      :lyt_do -> true
      :lyt_ado -> true
      _ -> false
    end
  end



  def current_indent_go(stk) do
    case stk do
      [{:tuple, pos, lyt} | rest] -> if is_indented(lyt) do
          {:just, pos}
        else
          current_indent_go(rest)
        end
      _ -> :nothing
    end
  end



  def current_indent(auto_arg0) do
    current_indent_go(auto_arg0)
  end



  def is_top_decl(tok_pos, stack) do
    case stack do
      [{:tuple, lyt_pos, lyt1} | rest] -> if (lyt1 == :lyt_where) do
          case rest do
            [{:tuple, _, lyt2} | []] -> ((lyt2 == :lyt_root) and (tok_pos.column == lyt_pos.column))
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
    
      tok_pos = src.range.start
      tok = src.value
      find_indented = Nova.Runtime.fix(fn find_indented -> fn auto_arg0 -> case auto_arg0 do
        [] -> :nothing
        ([({:tuple, _, lyt}) = item | rest]) -> if is_indented(lyt) do
  {:just, item}
else
  find_indented.(rest)
end
      end end end)
      insert_token = fn token -> fn ({:tuple, stk, acc}) -> {:tuple, stk, (Nova.Runtime.append(acc, ([({:tuple, token, stk}) | []])))} end end
      push_stack = fn lyt_pos -> fn lyt -> fn ({:tuple, stk, acc}) -> {:tuple, ([{:tuple, lyt_pos, lyt} | stk]), acc} end end end
      pop_stack = fn p -> fn state -> case state do
        {:tuple, ([{:tuple, _, lyt} | stk_prime]), acc} -> if p.(lyt) do
            {:tuple, stk_prime, acc}
          else
            state
          end
        _ -> state
      end end end
      indented_p = Nova.Runtime.const((&is_indented/1))
      in_p = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {_, :lyt_let} -> false
        {_, :lyt_ado} -> false
        {_, lyt} -> is_indented(lyt)
      end end end
      guard_p = fn auto_arg0 -> case auto_arg0 do
        :lyt_case_binders -> true
        :lyt_case_guard -> true
        :lyt_lambda_binders -> true
        _ -> false
      end end
      equals_p = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {_, :lyt_where} -> true
        {_, :lyt_let} -> true
        {_, :lyt_let_stmt} -> true
        {_, _} -> false
      end end end
      insert_start = fn lyt -> fn ({:tuple, stk, _}) = state -> case find_indented.(stk) do
        {:just, ({:tuple, pos, _})} -> if (next_pos.column <= pos.column) do
            state
          else
            (insert_token.((lyt_token(next_pos, (Cst.tok_layout_start(next_pos.column)))))).((push_stack.(next_pos).(lyt)).(state))
          end
        _ -> (insert_token.((lyt_token(next_pos, (Cst.tok_layout_start(next_pos.column)))))).((push_stack.(next_pos).(lyt)).(state))
      end end end
      insert_end = fn indent -> insert_token.((lyt_token(tok_pos, (Cst.tok_layout_end(indent))))) end
      collapse = fn p -> 
        go = Nova.Runtime.fix(fn go -> fn state -> case state do
          {:tuple, ([{:tuple, lyt_pos, lyt} | stk_prime]), acc} -> if p.(lyt_pos).(lyt) do
              go.((({:tuple, stk_prime}).(if is_indented(lyt) do
  Nova.Runtime.append(acc, ([({:tuple, (lyt_token(tok_pos, (Cst.tok_layout_end(lyt_pos.column)))), stk_prime}) | []]))
else
  acc
end)))
            else
              state
            end
          _ -> state
        end  end end)
        go end
      offside_p = fn lyt_pos -> fn lyt -> (is_indented(lyt) and (tok_pos.column < lyt_pos.column)) end end
      offside_end_p = fn lyt_pos -> fn lyt -> (is_indented(lyt) and (tok_pos.column <= lyt_pos.column)) end end
      sep_p = fn lyt_pos -> ((tok_pos.column == lyt_pos.column) and (tok_pos.line != lyt_pos.line)) end
      indent_sep_p = fn lyt_pos -> fn lyt -> (is_indented(lyt) and sep_p.(lyt_pos)) end end
      where_p = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {_, :lyt_do} -> true
        {lyt_pos, lyt} -> offside_end_p.(lyt_pos).(lyt)
      end end end
      let_next = fn ({:tuple, stk_prime, _}) = state_prime -> case stk_prime do
        [{:tuple, p, :lyt_do} | _] -> if (p.column == tok_pos.column) do
            (insert_start.(:lyt_let_stmt)).(state_prime)
          else
            (insert_start.(:lyt_let)).(state_prime)
          end
        [{:tuple, p, :lyt_ado} | _] -> if (p.column == tok_pos.column) do
            (insert_start.(:lyt_let_stmt)).(state_prime)
          else
            (insert_start.(:lyt_let)).(state_prime)
          end
        _ -> (insert_start.(:lyt_let)).(state_prime)
      end end
      arrow_p = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {_, :lyt_do} -> true
        {_, :lyt_of} -> false
        {lyt_pos, lyt} -> offside_end_p.(lyt_pos).(lyt)
      end end end
      insert_sep = fn ({:tuple, stk, acc}) = state -> 
        sep_tok = lyt_token(tok_pos, (Cst.tok_layout_sep(tok_pos.column)))
        case stk do
  [{:tuple, lyt_pos, :lyt_top_decl} | stk_prime] -> if sep_p.(lyt_pos) do
      (insert_token.(sep_tok)).({:tuple, stk_prime, acc})
    else
      state
    end
  [{:tuple, lyt_pos, :lyt_top_decl_head} | stk_prime] -> if sep_p.(lyt_pos) do
      (insert_token.(sep_tok)).({:tuple, stk_prime, acc})
    else
      state
    end
  [{:tuple, lyt_pos, lyt} | _] -> if indent_sep_p.(lyt_pos).(lyt) do
      case lyt do
        :lyt_of -> (push_stack.(tok_pos).(:lyt_case_binders)).((insert_token.(sep_tok)).(state))
        _ -> (insert_token.(sep_tok)).(state)
      end
    else
      state
    end
  _ -> state
end end
      insert_default = fn state -> (insert_token.(src)).((insert_sep).((collapse.(offside_p)).(state))) end
      insert_kw_property = fn k -> fn state -> case (insert_default).(state) do
        {:tuple, ([{:tuple, _, :lyt_property} | stk_prime]), acc_prime} -> {:tuple, stk_prime, acc_prime}
        state_prime -> k.(state_prime)
      end end end
      insert = fn state -> case tok do
        {:tok_lower_name, :nothing, "data"} -> case (insert_default).(state) do
            ({:tuple, stk_prime, _}) = state_prime ->
              if is_top_decl(tok_pos, stk_prime) do
                (push_stack.(tok_pos).(:lyt_top_decl)).(state_prime)
              end
            state_prime -> (pop_stack.((fn __x__ -> (__x__ == :lyt_property) end))).(state_prime)
          end
        {:tok_lower_name, :nothing, "class"} -> case (insert_default).(state) do
            ({:tuple, stk_prime, _}) = state_prime ->
              if is_top_decl(tok_pos, stk_prime) do
                (push_stack.(tok_pos).(:lyt_top_decl_head)).(state_prime)
              end
            state_prime -> (pop_stack.((fn __x__ -> (__x__ == :lyt_property) end))).(state_prime)
          end
        {:tok_lower_name, :nothing, "where"} -> case stack do
            [{:tuple, _, :lyt_top_decl_head} | stk_prime] -> (insert_start.(:lyt_where)).((insert_token.(src)).({:tuple, stk_prime, (Nova.Runtime.snd(state))}))
            [{:tuple, _, :lyt_property} | stk_prime] -> (insert_token.(src)).({:tuple, stk_prime, (Nova.Runtime.snd(state))})
            _ -> (insert_start.(:lyt_where)).((insert_token.(src)).((collapse.(where_p)).(state)))
          end
        {:tok_lower_name, :nothing, "in"} -> case collapse.(in_p).(state) do
            {:tuple, ([[({:tuple, pos1, :lyt_let_stmt}) | ({:tuple, pos2, :lyt_ado})] | stk_prime]), acc_prime} -> (insert_token.(src)).((insert_end.(pos2.column)).((insert_end.(pos1.column)).({:tuple, stk_prime, acc_prime})))
            {:tuple, ([{:tuple, pos1, lyt} | stk_prime]), acc_prime} -> if is_indented(lyt) do
                (insert_token.(src)).((insert_end.(pos1.column)).({:tuple, stk_prime, acc_prime}))
              else
                (pop_stack.((fn __x__ -> (__x__ == :lyt_property) end))).((insert_default).(state))
              end
            _ -> (pop_stack.((fn __x__ -> (__x__ == :lyt_property) end))).((insert_default).(state))
          end
        {:tok_lower_name, :nothing, "let"} -> (insert_kw_property.(let_next)).(state)
        {:tok_lower_name, _, "do"} -> (insert_kw_property.((insert_start.(:lyt_do)))).(state)
        {:tok_lower_name, _, "ado"} -> (insert_kw_property.((insert_start.(:lyt_ado)))).(state)
        {:tok_lower_name, :nothing, "case"} -> (insert_kw_property.((push_stack.(tok_pos).(:lyt_case)))).(state)
        {:tok_lower_name, :nothing, "of"} -> case collapse.(indented_p).(state) do
            {:tuple, ([{:tuple, _, :lyt_case} | stk_prime]), acc_prime} -> (push_stack.(next_pos).(:lyt_case_binders)).((insert_start.(:lyt_of)).((insert_token.(src)).({:tuple, stk_prime, acc_prime})))
            state_prime -> (pop_stack.((fn __x__ -> (__x__ == :lyt_property) end))).((insert_default).(state_prime))
          end
        {:tok_lower_name, :nothing, "if"} -> (insert_kw_property.((push_stack.(tok_pos).(:lyt_if)))).(state)
        {:tok_lower_name, :nothing, "then"} -> case (collapse.(indented_p)).(state) do
            {:tuple, ([{:tuple, _, :lyt_if} | stk_prime]), acc_prime} -> (push_stack.(tok_pos).(:lyt_then)).((insert_token.(src)).({:tuple, stk_prime, acc_prime}))
            _ -> (pop_stack.((fn __x__ -> (__x__ == :lyt_property) end))).((insert_default).(state))
          end
        {:tok_lower_name, :nothing, "else"} -> case (collapse.(indented_p)).(state) do
            {:tuple, ([{:tuple, _, :lyt_then} | stk_prime]), acc_prime} -> (insert_token.(src)).({:tuple, stk_prime, acc_prime})
            _ -> case (collapse.(offside_p)).(state) do
                ({:tuple, stk_prime, _}) = state_prime ->
                  if is_top_decl(tok_pos, stk_prime) do
                    (insert_token.(src)).(state_prime)
                  end
                state_prime -> (pop_stack.((fn __x__ -> (__x__ == :lyt_property) end))).((insert_token.(src)).((insert_sep).(state_prime)))
              end
          end
        :tok_forall -> (insert_kw_property.((push_stack.(tok_pos).(:lyt_forall)))).(state)
        :tok_backslash -> (push_stack.(tok_pos).(:lyt_lambda_binders)).((insert_default).(state))
        :tok_right_arrow -> (insert_token.(src)).((pop_stack.(guard_p)).((collapse.(arrow_p)).(state)))
        :tok_equals -> case (collapse.(equals_p)).(state) do
            {:tuple, ([{:tuple, _, :lyt_decl_guard} | stk_prime]), acc_prime} -> (insert_token.(src)).({:tuple, stk_prime, acc_prime})
            _ -> (insert_default).(state)
          end
        :tok_pipe -> case collapse.(offside_end_p).(state) do
            ({:tuple, ([{:tuple, _, :lyt_of} | _]), _}) = state_prime -> (insert_token.(src)).((push_stack.(tok_pos).(:lyt_case_guard)).(state_prime))
            ({:tuple, ([{:tuple, _, :lyt_let} | _]), _}) = state_prime -> (insert_token.(src)).((push_stack.(tok_pos).(:lyt_decl_guard)).(state_prime))
            ({:tuple, ([{:tuple, _, :lyt_let_stmt} | _]), _}) = state_prime -> (insert_token.(src)).((push_stack.(tok_pos).(:lyt_decl_guard)).(state_prime))
            ({:tuple, ([{:tuple, _, :lyt_where} | _]), _}) = state_prime -> (insert_token.(src)).((push_stack.(tok_pos).(:lyt_decl_guard)).(state_prime))
            _ -> (insert_default).(state)
          end
        :tok_tick -> case (collapse.(indented_p)).(state) do
            {:tuple, ([{:tuple, _, :lyt_tick} | stk_prime]), acc_prime} -> (insert_token.(src)).({:tuple, stk_prime, acc_prime})
            _ -> (push_stack.(tok_pos).(:lyt_tick)).((insert_token.(src)).((insert_sep).((collapse.(offside_end_p)).(state))))
          end
        :tok_comma -> case (collapse.(indented_p)).(state) do
            ({:tuple, ([{:tuple, _, :lyt_brace} | _]), _}) = state_prime -> (push_stack.(tok_pos).(:lyt_property)).((insert_token.(src)).(state_prime))
            state_prime -> (insert_token.(src)).(state_prime)
          end
        :tok_dot -> case (insert_default).(state) do
            {:tuple, ([{:tuple, _, :lyt_forall} | stk_prime]), acc_prime} -> {:tuple, stk_prime, acc_prime}
            state_prime -> (push_stack.(tok_pos).(:lyt_property)).(state_prime)
          end
        :tok_left_paren -> (push_stack.(tok_pos).(:lyt_paren)).((insert_default).(state))
        :tok_left_brace -> (push_stack.(tok_pos).(:lyt_property)).((push_stack.(tok_pos).(:lyt_brace)).((insert_default).(state)))
        :tok_left_square -> (push_stack.(tok_pos).(:lyt_square)).((insert_default).(state))
        :tok_right_paren -> (insert_token.(src)).((pop_stack.((fn __x__ -> (__x__ == :lyt_paren) end))).((collapse.(indented_p)).(state)))
        :tok_right_brace -> (insert_token.(src)).((pop_stack.((fn __x__ -> (__x__ == :lyt_brace) end))).((pop_stack.((fn __x__ -> (__x__ == :lyt_property) end))).((collapse.(indented_p)).(state))))
        :tok_right_square -> (insert_token.(src)).((pop_stack.((fn __x__ -> (__x__ == :lyt_square) end))).((collapse.(indented_p)).(state)))
        {:tok_string, _, _} -> (pop_stack.((fn __x__ -> (__x__ == :lyt_property) end))).((insert_default).(state))
        {:tok_lower_name, :nothing, _} -> (pop_stack.((fn __x__ -> (__x__ == :lyt_property) end))).((insert_default).(state))
        {:tok_operator, _, _} -> (insert_token.(src)).((insert_sep).((collapse.(offside_end_p)).(state)))
        _ -> (insert_default).(state)
      end end
      insert.(({:tuple, stack, []}))
  end
end
