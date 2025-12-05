defmodule Nova.Compiler.Tokenizer do
  # import Prelude

  # import Data.Array

  # import Data.String

  # import Data.String.CodeUnits

  # import Data.Maybe

  # import Data.Either

  # import Data.Tuple

  # Data type: TokenType
  def tok_keyword(), do: :tok_keyword
  def tok_identifier(), do: :tok_identifier
  def tok_number(), do: :tok_number
  def tok_string(), do: :tok_string
  def tok_char(), do: :tok_char
  def tok_operator(), do: :tok_operator
  def tok_delimiter(), do: :tok_delimiter
  def tok_newline(), do: :tok_newline
  def tok_unrecognized(), do: :tok_unrecognized

  # derive instance Eq token_type()

  # @type token :: %{token_type: token_type(), value: string(), line: int(), column: int(), pos: int()}



  def mk_token(token_type, value, line, column, pos) do
    %{token_type: token_type, value: value, line: line, column: column, pos: pos}
  end



  def keywords() do
    ["foreign", "module", "where", "import", "data", "type", "class", "instance", "let", "in", "if", "then", "else", "case", "of", "do", "derive", "newtype", "infixl", "infixr", "infix", "forall"]
  end



  def operators() do
    ["==", "/=", "!=", "<=", ">=", "->", "<-", "::", "++", "++=", ">>=", ">>>", "<<<", ">>", "<<", "&&", "||", "<>", "..", "+", "-", "*", "/", "<", ">", "=", "$", "`", ".", "|", "\\", "&", ":", "@"]
  end



  def is_operator_char(c) do
    Nova.Array.elem(c, [?+, ?-, ?*, ?/, ?=, ?<, ?>, ?!, ?:, ?., ?|, ?\\, ?&, ?$, ?`, ?#, ?@])
  end



  def is_delimiter(c) do
    Nova.Array.elem(c, [?(, ?), ?{, ?}, ?[, ?], ?,, ?;])
  end



  def is_alpha(c) do
    (((((c >= ?a)) and ((c <= ?z)))) or ((((c >= ?A)) and ((c <= ?Z)))))
  end



  def is_digit(c) do
    (((c >= ?0)) and ((c <= ?9)))
  end



  def is_ident_char(c) do
    (((is_alpha(c) or is_digit(c)) or ((c == ?_))) or ((c == ?')))
  end

  # @type tok_state :: %{input: string(), chars: array()(char()), pos: int(), line: int(), column: int()}



  def init_state(input) do
    %{input: input, chars: Nova.String.to_char_array(input), pos: 0, line: 1, column: 1}
  end



  def tokenize(source) do
    
      go = Nova.Runtime.fix2(fn go -> fn state -> fn acc -> case next_token(state) do
        :nothing -> Nova.Array.reverse(acc)
        {:just, ({:tuple, tok, state_prime})} -> go.(state_prime).((Nova.Array.cons(tok, acc)))
      end  end end end)
      go.((init_state(source))).([])
  end



  def next_token(state) do
      case peek(state) do
    :nothing -> :nothing
    {:just, c} ->
      case c do
  ?\s -> next_token((advance(state, 1)))
  ?\t -> next_token((advance_tab(state)))
  ?\r -> case peek_at(state, 1) do
      {:just, ?\n} -> {:just, ({:tuple, (mk_token(:tok_newline, "\n", state.line, state.column, state.pos)), (advance_newline((advance(state, 2))))})}
      _ -> {:just, ({:tuple, (mk_token(:tok_newline, "\n", state.line, state.column, state.pos)), (advance_newline((advance(state, 1))))})}
    end
  ?\n -> {:just, ({:tuple, (mk_token(:tok_newline, "\n", state.line, state.column, state.pos)), (advance_newline((advance(state, 1))))})}
  ?- -> case peek_at(state, 1) do
      {:just, ?-} -> 
          state_prime = consume_line_comment((advance(state, 2)))
          state_prime_prime = case peek(state_prime) do
            {:just, ?\n} -> advance(state_prime, 1)
            {:just, ?\r} -> case peek_at(state_prime, 1) do
                {:just, ?\n} -> advance(state_prime, 2)
                _ -> advance(state_prime, 1)
              end
            _ -> state_prime
          end
          {:just, ({:tuple, (mk_token(:tok_newline, "\n", state.line, state.column, state.pos)), (advance_newline(state_prime_prime))})}
      _ -> tokenize_operator(state)
    end
  ?{ -> case peek_at(state, 1) do
      {:just, ?-} -> next_token((consume_block_comment((advance(state, 2)), 1)))
      _ -> tokenize_delimiter(state)
    end
  ?" -> tokenize_string(state)
  ?' -> tokenize_char(state)
  _ ->
    cond do
      is_digit(c) -> tokenize_number(state)
      (is_alpha(c) or ((c == ?_))) -> tokenize_identifier(state)
      is_operator_char(c) -> tokenize_operator(state)
      is_delimiter(c) -> tokenize_delimiter(state)
      true -> {:just, ({:tuple, (mk_token(:tok_unrecognized, (Nova.String.singleton(c)), state.line, state.column, state.pos)), (advance(state, 1))})}
    end
  _ -> {:just, ({:tuple, (mk_token(:tok_unrecognized, (Nova.String.singleton(c)), state.line, state.column, state.pos)), (advance(state, 1))})}
end
  end
  end



  def peek(state) do
    Nova.Array.head(state.chars)
  end



  def peek_at(state, offset) do
    Nova.Array.index(state.chars, offset)
  end



  def advance(state, n) do
    %{state | pos: (state.pos + n), column: (state.column + n), chars: Nova.Array.drop(n, state.chars)}
  end



  def advance_tab(state) do
    
      next_col = (state.column + ((8 - (rem(((state.column - 1)), 8)))))
      %{state | pos: (state.pos + 1), column: next_col, chars: Nova.Array.drop(1, state.chars)}
  end



  def advance_newline(state) do
    %{state | line: (state.line + 1), column: 1}
  end



  def consume_line_comment(state) do
    case peek(state) do
      :nothing -> state
      {:just, ?\n} -> state
      {:just, ?\r} -> state
      {:just, _} -> consume_line_comment((advance(state, 1)))
    end
  end



  def consume_block_comment(state, 0) do
    state
  end

  def consume_block_comment(state, depth) do
    case peek(state) do
      :nothing -> state
      {:just, ?-} -> case peek_at(state, 1) do
          {:just, ?}} -> consume_block_comment((advance(state, 2)), ((depth - 1)))
          _ -> consume_block_comment((advance(state, 1)), depth)
        end
      {:just, ?{} -> case peek_at(state, 1) do
          {:just, ?-} -> consume_block_comment((advance(state, 2)), ((depth + 1)))
          _ -> consume_block_comment((advance(state, 1)), depth)
        end
      {:just, ?\n} -> consume_block_comment((advance_newline((advance(state, 1)))), depth)
      {:just, _} -> consume_block_comment((advance(state, 1)), depth)
    end
  end



  def tokenize_string(state) do
    
      start_line = state.line
      start_col = state.column
      start_pos = state.pos
      {:tuple, str, state_prime} = consume_string((advance(state, 1)), "")
      tok = mk_token(:tok_string, str, start_line, start_col, start_pos)
      {:just, ({:tuple, tok, state_prime})}
  end



  def consume_string(state, acc) do
    case peek(state) do
      :nothing -> {:tuple, acc, state}
      {:just, ?"} -> {:tuple, acc, (advance(state, 1))}
      {:just, ?\\} -> case peek_at(state, 1) do
          {:just, c} -> 
              escaped = case c do
                ?n -> "\n"
                ?t -> "\t"
                ?r -> ""
                ?\\ -> "\\"
                ?" -> "\""
                _ -> Nova.String.singleton(c)
              end
              consume_string((advance(state, 2)), (Nova.Runtime.append(acc, escaped)))
          :nothing -> {:tuple, acc, state}
        end
      {:just, ?\n} -> consume_string((advance_newline((advance(state, 1)))), (Nova.Runtime.append(acc, "\n")))
      {:just, c} -> consume_string((advance(state, 1)), (Nova.Runtime.append(acc, Nova.String.singleton(c))))
    end
  end



  def tokenize_char(state) do
    
      start_line = state.line
      start_col = state.column
      start_pos = state.pos
      state1 = advance(state, 1)
      case peek(state1) do
  {:just, ?\\} -> case peek_at(state1, 1) do
      {:just, c} -> case peek_at(state1, 2) do
          {:just, ?'} -> 
              val = case c do
                ?n -> "\n"
                ?t -> "\t"
                ?r -> ""
                ?\\ -> "\\"
                ?' -> "'"
                _ -> Nova.String.singleton(c)
              end
              {:just, ({:tuple, (mk_token(:tok_char, val, start_line, start_col, start_pos)), (advance(state1, 3))})}
          _ -> {:just, ({:tuple, (mk_token(:tok_unrecognized, "'", start_line, start_col, start_pos)), state1})}
        end
      _ -> {:just, ({:tuple, (mk_token(:tok_unrecognized, "'", start_line, start_col, start_pos)), state1})}
    end
  {:just, c} -> case peek_at(state1, 1) do
      {:just, ?'} -> {:just, ({:tuple, (mk_token(:tok_char, (Nova.String.singleton(c)), start_line, start_col, start_pos)), (advance(state1, 2))})}
      _ -> {:just, ({:tuple, (mk_token(:tok_unrecognized, "'", start_line, start_col, start_pos)), state1})}
    end
  :nothing -> {:just, ({:tuple, (mk_token(:tok_unrecognized, "'", start_line, start_col, start_pos)), state1})}
end
  end



  def tokenize_number(state) do
    
      start_line = state.line
      start_col = state.column
      start_pos = state.pos
      {:tuple, num, state_prime} = consume_number(state, "")
      {:just, ({:tuple, (mk_token(:tok_number, num, start_line, start_col, start_pos)), state_prime})}
  end



  def consume_number(state, acc) do
    case peek(state) do
      {:just, c} ->
        cond do
          is_digit(c) -> consume_number((advance(state, 1)), (Nova.Runtime.append(acc, Nova.String.singleton(c))))
          true -> case peek_at(state, 1) do
              {:just, d} ->
                cond do
                  is_digit(d) -> consume_number((advance(state, 2)), (Nova.Runtime.append(Nova.Runtime.append(acc, "."), Nova.String.singleton(d))))
                  true -> {:tuple, acc, state}
                end
              _ -> {:tuple, acc, state}
            end
        end
      _ -> {:tuple, acc, state}
    end
  end



  def tokenize_identifier(state) do
    
      start_line = state.line
      start_col = state.column
      start_pos = state.pos
      {:tuple, ident, state_prime} = consume_ident(state, "")
      tok_type = if Nova.Array.elem(ident, keywords()) do
        :tok_keyword
      else
        :tok_identifier
      end
      {:just, ({:tuple, (mk_token(tok_type, ident, start_line, start_col, start_pos)), state_prime})}
  end



  def consume_ident(state, acc) do
    case peek(state) do
      {:just, c} ->
        cond do
          is_ident_char(c) -> consume_ident((advance(state, 1)), (Nova.Runtime.append(acc, Nova.String.singleton(c))))
          true -> {:tuple, acc, state}
        end
      _ -> {:tuple, acc, state}
    end
  end



  def tokenize_operator(state) do
    
      start_line = state.line
      start_col = state.column
      start_pos = state.pos
      prefix = Nova.String.from_char_array((Nova.Array.take(3, state.chars)))
      tok_type = :tok_operator
      op = find_operator(prefix, operators())
      len = Nova.String.length(op)
      {:just, ({:tuple, (mk_token(tok_type, op, start_line, start_col, start_pos)), (advance(state, len))})}
  end



  def find_operator(input, ops) do
    case Nova.Array.find((fn op -> (Nova.String.take((Nova.String.length(op)), input) == op) end), ops) do
      {:just, op} -> op
      :nothing -> Nova.String.take(1, input)
    end
  end



  def tokenize_delimiter(state) do
      case peek(state) do
    :nothing -> :nothing
    {:just, c} ->
            tok = mk_token(:tok_delimiter, (Nova.String.singleton(c)), state.line, state.column, state.pos)
      {:just, ({:tuple, tok, (advance(state, 1))})}
  end
  end
end
