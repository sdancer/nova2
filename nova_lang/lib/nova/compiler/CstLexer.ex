defmodule Nova.Compiler.CstLexer do
  # import Prelude

  # import Data.Array

  # import Data.String

  # import Data.String.CodeUnits

  # import Data.Maybe

  # import Data.Tuple

  # import Data.Int

  # import Data.Number

  # import Data.List

  # import Nova.Compiler.Cst

  # import Nova.Compiler.CstLayout

  # @type lex_state :: %{input: string(), chars: array()(char()), pos: int(), line: int(), column: int()}



  def init_lex_state(input) do
    %{input: input, chars: Nova.String.to_char_array(input), pos: 0, line: 1, column: 1}
  end



  def keywords() do
    ["foreign", "module", "where", "import", "data", "type", "class", "instance", "let", "in", "if", "then", "else", "case", "of", "do", "ado", "derive", "newtype", "infixl", "infixr", "infix", "forall", "as", "hiding", "true", "false", "role", "nominal", "representational", "phantom"]
  end



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



  def lyt_token(pos, value) do
    %{range: %{start: pos, end_: pos}, leading_comments: [], trailing_comments: [], value: value}
  end



  def close_layouts() do
    fn auto_p0 -> close_layouts_go([], auto_p0) end
  end



  def close_layouts_go(acc, :nil_) do
    acc
  end

  def close_layouts_go(acc, ([{:tuple, pos, lyt} | rest])) do
    cond do
      (is_indented(lyt)) ->
        
  end_tok = lyt_token(pos, (Cst.tok_layout_end(pos.column)))
  close_layouts_go((Nova.Runtime.append(acc, ([end_tok | []]))), rest)
      (true) ->
        close_layouts_go(acc, rest)
    end
  end



  def insert_layout_go(stack, toks, acc) do
    case toks do
      :nil_ -> Nova.Runtime.append(acc, close_layouts(stack))
      ([tok | rest]) -> 
          next_pos = case rest do
            ([next | _]) -> next.range.start
            :nil_ -> %{line: (tok.range.end_.line + 1), column: 1}
          end
          new_toks = Nova.Runtime.map((&Nova.Runtime.fst/1), output_tokens)
          {:tuple, new_stack, output_tokens} = insert_layout.(tok).(next_pos).(stack)
          insert_layout_go(new_stack, rest, (Nova.Runtime.append(acc, new_toks)))
    end
  end



  def insert_layout_tokens(tokens) do
    insert_layout_go(init_stack, tokens, [])
  end



  def lex_module(source) do
    
      raw_tokens = lex_tokens(source)
      insert_layout_tokens(raw_tokens)
  end



  def lex_tokens(source) do
    lex_tokens_go((init_lex_state(source)), [])
  end



  def lex_tokens_go(state, acc) do
    case lex_token(state) do
      :nothing -> acc
      {:just, ({:tuple, tok, state_prime})} -> lex_tokens_go(state_prime, (Nova.Runtime.append(acc, ([tok | []]))))
    end
  end



  def lex_token(state) do
      case peek(state) do
    :nothing -> :nothing
    {:just, c} ->
      case c do
  ?\s -> lex_token((advance(state, 1)))
  ?\t -> lex_token((advance_tab(state)))
  ?\n -> lex_token((advance_newline((advance(state, 1)))))
  ?\r -> case peek_at(state, 1) do
      {:just, ?\n} -> lex_token((advance_newline((advance(state, 2)))))
      _ -> lex_token((advance_newline((advance(state, 1)))))
    end
  ?- -> case peek_at(state, 1) do
      {:just, ?-} -> 
          state_prime = skip_line_comment.((advance(state, 2)))
          lex_token(state_prime)
      _ -> lex_operator.(state)
    end
  ?{ -> case peek_at(state, 1) do
      {:just, ?-} -> 
          state_prime = skip_block_comment.((advance(state, 2))).(1)
          lex_token(state_prime)
      _ -> lex_delimiter.(state)
    end
  ?" -> case peek_at(state, 1) do
      {:just, ?"} -> case peek_at(state, 2) do
          {:just, ?"} -> lex_raw_string.(state)
          _ -> lex_string.(state)
        end
      _ -> lex_string.(state)
    end
  ?' -> lex_char.(state)
  _ ->
    cond do
      is_digit(c) -> lex_number.(state)
      is_ident_start(c) -> lex_ident_or_keyword.(state)
      is_upper(c) -> lex_upper_identifier.(state)
      is_operator_char(c) -> lex_operator.(state)
    end
  ?( -> lex_delimiter.(state)
  ?) -> lex_delimiter.(state)
  ?[ -> lex_delimiter.(state)
  ?] -> lex_delimiter.(state)
  ?} -> lex_delimiter.(state)
  ?, -> lex_delimiter.(state)
  ?` -> lex_delimiter.(state)
  ?; -> lex_delimiter.(state)
  ?_ -> lex_underscore.(state)
  ?@ -> {:just, ({:tuple, (make_token(Cst.tok_at, state)), (advance(state, 1))})}
  _ -> lex_token((advance(state, 1)))
end
  end
  end



  def is_digit(c) do
    (((c >= ?0) and c) <= ?9)
  end



  def is_upper(c) do
    (((c >= ?A) and c) <= ?Z)
  end



  def is_lower(c) do
    (((c >= ?a) and c) <= ?z)
  end



  def is_alpha(c) do
    (is_upper(c) or is_lower(c))
  end



  def is_alpha_num(c) do
    (is_alpha(c) or is_digit(c))
  end



  def is_ident_start(c) do
    ((is_lower(c) or c) == ?_)
  end



  def is_ident_char(c) do
    ((((is_alpha_num(c) or c) == ?_) or c) == ?')
  end



  def is_operator_char(c) do
    (((((((((((((((((((((((((((((((((((((c == ?:) or c) == ?!) or c) == ?#) or c) == ?$) or c) == ?%) or c) == ?&) or c) == ?*) or c) == ?+) or c) == ?.) or c) == ?/) or c) == ?<) or c) == ?=) or c) == ?>) or c) == ??) or c) == ?\\) or c) == ?^) or c) == ?|) or c) == ?-) or c) == ?~)
  end



  def peek(state) do
    Nova.Array.index(state.chars, state.pos)
  end



  def peek_at(state, offset) do
    Nova.Array.index(state.chars, ((state.pos + offset)))
  end



  def advance(state, n) do
    state.(%{pos: (state.pos + n), column: (state.column + n)})
  end



  def advance_newline(state) do
    state.(%{line: (state.line + 1), column: 1})
  end



  def advance_tab(state) do
    
      col = state.column
      next_tab = (((((((col - 1)) / 8) + 1)) * 8) + 1)
      state.(%{pos: (state.pos + 1), column: next_tab})
  end



  def make_token(tok, state) do
    %{range: %{start: %{line: state.line, column: state.column}, end_: %{line: state.line, column: state.column}}, leading_comments: [], trailing_comments: [], value: tok}
  end



  def make_token_range(tok, start_state, end_state) do
    %{range: %{start: %{line: start_state.line, column: start_state.column}, end_: %{line: end_state.line, column: end_state.column}}, leading_comments: [], trailing_comments: [], value: tok}
  end


end
