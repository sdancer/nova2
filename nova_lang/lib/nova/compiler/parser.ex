defmodule Nova.Compiler.Parser do
  # import Prelude

  # import Data.Array

  # import Data.Maybe

  # import Data.Either

  # import Data.Tuple

  # import Data.Foldable

  # import Data.String

  # import Data.String.CodeUnits

  # import Data.Int

  # import Data.Number

  # import Nova.Compiler.Tokenizer

  # import Nova.Compiler.Ast

  # @type parse_result :: either()(string())(tuple()(a)(array()(token())))

  def success(a, tokens) do
    {:right, {:tuple, a, tokens}}
  end

  def failure(msg) do
    {:left, msg}
  end

  def skip_newlines(tokens) do
    Nova.Array.drop_while(fn t -> (t.token_type == :tok_newline) end, tokens)
  end

  def drop_newlines(__arg0__) do
    skip_newlines(__arg0__)
  end

  def strip_newlines(__arg0__) do
    Nova.Array.filter(fn t -> (t.token_type != :tok_newline) end, __arg0__)
  end

  def is_lower_case(s) do
    case Nova.String.char_at(0, s) do
      {:just, c} -> ((c >= ?a) and (c <= ?z))
      :nothing -> false
    end
  end

  def expect_keyword(tokens, expected) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_keyword) and (t.value == expected)) do
      success(expected, Nova.Array.drop(1, ts))
    else
      failure(Nova.Runtime.append("Expected keyword '", Nova.Runtime.append(expected, "'")))
    end
  :nothing -> failure(Nova.Runtime.append("Expected keyword '", Nova.Runtime.append(expected, "'")))
end
  end

  def expect_operator(tokens, expected) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == expected)) do
      success(expected, Nova.Array.drop(1, ts))
    else
      failure(Nova.Runtime.append("Expected operator '", Nova.Runtime.append(expected, "'")))
    end
  :nothing -> failure(Nova.Runtime.append("Expected operator '", Nova.Runtime.append(expected, "'")))
end
  end

  def expect_delimiter(tokens, expected) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == expected)) do
      success(expected, Nova.Array.drop(1, ts))
    else
      failure(Nova.Runtime.append("Expected delimiter '", Nova.Runtime.append(expected, "'")))
    end
  :nothing -> failure(Nova.Runtime.append("Expected delimiter '", Nova.Runtime.append(expected, "'")))
end
  end

  def expect_colon(tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == ":")) do
          success(":", Nova.Array.drop(1, tokens))
        else
          failure("Expected ':'  operator")
        end
      :nothing -> failure("Expected ':'  operator")
    end
  end

  def parse_identifier(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (t.token_type == :tok_identifier) do
      success(Nova.Compiler.Ast.expr_var(t.value), Nova.Array.drop(1, ts))
    else
      failure("Expected identifier")
    end
  :nothing -> failure("Expected identifier")
end
  end

  def parse_identifier_name(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (t.token_type == :tok_identifier) do
      success(t.value, Nova.Array.drop(1, ts))
    else
      failure("Expected identifier")
    end
  :nothing -> failure("Expected identifier")
end
  end

  def parse_label(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_identifier) or (t.token_type == :tok_keyword)) do
      success(t.value, Nova.Array.drop(1, ts))
    else
      failure("Expected label")
    end
  :nothing -> failure("Expected label")
end
  end

  def parse_literal(tokens) do
    
      read_int = fn s -> case Nova.String.to_int(s) do
        {:just, n} -> n
        :nothing -> 0
      end end
      read_number = fn s -> case Nova.String.to_float(s) do
        {:just, n} -> n
        :nothing -> 0.0
      end end
      first_char = fn s -> case Nova.String.char_at(0, s) do
        {:just, c} -> c
        :nothing -> ?\s
      end end
      
  ts = skip_newlines(tokens)
  case Nova.Array.head(ts) do
  {:just, t} -> case t.token_type do
      :tok_number -> if Nova.String.contains(Nova.String.pattern("."), t.value) do
          success(Nova.Compiler.Ast.lit_number(read_number.(t.value)), Nova.Array.drop(1, ts))
        else
          success(Nova.Compiler.Ast.lit_int(read_int.(t.value)), Nova.Array.drop(1, ts))
        end
      :tok_string -> success(Nova.Compiler.Ast.lit_string(t.value), Nova.Array.drop(1, ts))
      :tok_char -> success(Nova.Compiler.Ast.lit_char(first_char.(t.value)), Nova.Array.drop(1, ts))
      :tok_identifier -> if (t.value == "true") do
          success(Nova.Compiler.Ast.lit_bool(true), Nova.Array.drop(1, ts))
        else
          if (t.value == "false") do
            success(Nova.Compiler.Ast.lit_bool(false), Nova.Array.drop(1, ts))
          else
            failure("Expected literal")
          end
        end
      _ -> failure("Expected literal")
    end
  :nothing -> failure("Expected literal")
end
  end

  def parse_string_literal(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (t.token_type == :tok_string) do
      success(t.value, Nova.Array.drop(1, ts))
    else
      failure("Expected string literal")
    end
  :nothing -> failure("Expected string literal")
end
  end

  def parse_any(parsers, tokens) do
    
      go = Nova.Runtime.fix(fn go -> fn ps -> case Nova.Array.head(ps) do
        :nothing -> failure("No parser succeeded")
        {:just, p} -> case p.(tokens) do
            {:right, result} -> {:right, result}
            {:left, _} -> go.(Nova.Array.drop(1, ps))
          end
      end  end end)
      go.(parsers)
  end

  def parse_many(parser, tokens) do
    
      go = Nova.Runtime.fix2(fn go -> fn toks -> fn acc -> case parser.(toks) do
        {:right, ({:tuple, result, rest})} -> go.(rest).(Nova.Array.snoc(acc, result))
        {:left, _} -> success(acc, toks)
      end  end end end)
      go.(tokens).([])
  end

  def parse_separated(parser, separator, tokens) do
    case parser.(tokens) do
      {:left, err} -> {:left, err}
      {:right, ({:tuple, first, rest})} -> parse_separated_rest(parser, separator, rest, [first])
    end
  end

  def parse_separated_rest(parser, separator, tokens, acc) do
    case separator.(tokens) do
      {:right, ({:tuple, _, rest})} -> case parser.(rest) do
          {:right, ({:tuple, item, rest_prime})} -> parse_separated_rest(parser, separator, rest_prime, Nova.Array.snoc(acc, item))
          {:left, _} -> failure("Expected item after separator")
        end
      {:left, _} -> success(acc, tokens)
    end
  end

  def parse_qualified_identifier(tokens) do
    
      is_upper_case = fn s -> case Nova.String.char_at(0, s) do
        {:just, c} -> ((c >= ?A) and (c <= ?Z))
        :nothing -> false
      end end
      parse_qualified_identifier_inner = fn toks -> case parse_separated((&parse_identifier_name/1), fn t -> expect_operator(t, ".") end, toks) do
        {:left, err} -> {:left, err}
        {:right, ({:tuple, parts, rest})} -> case Nova.Array.length(parts) do
            0 -> failure("Expected identifier")
            1 -> case Nova.Array.head(parts) do
                {:just, name} -> success(Nova.Compiler.Ast.expr_var(name), rest)
                :nothing -> failure("Expected identifier")
              end
            _ -> case Nova.Array.head(parts) do
                {:just, first} -> if not(is_upper_case.(first)) do
                    
                      base_expr = Nova.Compiler.Ast.expr_var(first)
                      fields = Nova.Array.drop(1, parts)
                      success(Nova.Array.foldl(fn a, b -> Nova.Compiler.Ast.expr_record_access(a, b) end, base_expr, fields), rest)
                  else
                    
                      all_but_last = Nova.Array.take((Nova.Array.length(parts) - 1), parts)
                      last_name = Nova.Array.last(parts)
                      case last_name do
  {:just, name} -> success(Nova.Compiler.Ast.expr_qualified(Nova.String.join_with(".", all_but_last), name), rest)
  :nothing -> failure("Expected qualified identifier")
end
                  end
                _ -> 
                    all_but_last = Nova.Array.take((Nova.Array.length(parts) - 1), parts)
                    last_name = Nova.Array.last(parts)
                    case last_name do
  {:just, name} -> success(Nova.Compiler.Ast.expr_qualified(Nova.String.join_with(".", all_but_last), name), rest)
  :nothing -> failure("Expected qualified identifier")
end
              end
          end
      end end
      
  tokens_prime = skip_newlines(tokens)
  case Nova.Array.head(tokens_prime) do
  {:just, t} -> if ((t.token_type == :tok_identifier) and (t.column == 1)) do
      case Nova.Array.head(Nova.Array.drop(1, tokens_prime)) do
        {:just, t_prime} -> if ((t_prime.token_type == :tok_operator) and (t_prime.value == "::")) do
            failure("Not consuming top-level type signature")
          else
            parse_qualified_identifier_inner.(tokens_prime)
          end
        _ -> parse_qualified_identifier_inner.(tokens_prime)
      end
    else
      parse_qualified_identifier_inner.(tokens_prime)
    end
  _ -> parse_qualified_identifier_inner.(tokens_prime)
end
  end

  def parse_type(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_keyword) and (t.value == "forall")) do
      parse_forall_type(ts)
    else
      parse_function_type(tokens)
    end
  :nothing -> parse_function_type(tokens)
end
  end

  def parse_forall_type(tokens) do
      case expect_keyword(tokens, "forall") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
      case parse_many((&parse_identifier_name/1), rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, vars, rest_prime}} ->
          case expect_operator(rest_prime, ".") do
            {:left, err} -> {:left, err}
            {:right, {:tuple, _, rest_prime_prime}} ->
              case parse_type(rest_prime_prime) do
                {:left, err} -> {:left, err}
                {:right, {:tuple, ty, rest_prime_prime_prime}} ->
success(Nova.Compiler.Ast.ty_expr_for_all(vars, ty), rest_prime_prime_prime)
              end
          end
      end
  end
  end

  def parse_function_type(tokens) do
      case parse_type_term(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
            ts = skip_newlines(rest)
case Nova.Array.head(ts) do
              {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "->")) do
                           case parse_function_type(Nova.Array.drop(1, ts)) do
           {:left, err} -> {:left, err}
           {:right, {:tuple, right, rest_prime}} ->
success(Nova.Compiler.Ast.ty_expr_arrow(left, right), rest_prime)
         end
                else
                  success(left, rest)
                end
              :nothing -> success(left, rest)
            end
  end
  end

  def parse_type_term(tokens) do
    parse_any([(&parse_record_type/1), (&parse_list_type/1), (&parse_tuple_type/1), (&parse_basic_type/1)], tokens)
  end

  def parse_record_type(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "{")) do
         case parse_separated((&parse_record_field/1), fn tok -> expect_delimiter(tok, ",") end, Nova.Array.drop(1, ts)) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, fields, rest}} ->
       case expect_delimiter(rest, "}") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.ty_expr_record(fields, :nothing), rest_prime)
       end
   end
    else
      failure("Expected record type")
    end
  :nothing -> failure("Expected record type")
end
  end

  def parse_record_field(tokens) do
      case parse_label(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, label, rest}} ->
      case expect_operator(rest, "::") do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, rest_prime}} ->
                    rest_prime_prime = skip_newlines(rest_prime)
          case parse_type(rest_prime_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, ty, rest_prime_prime_prime}} ->
success({:tuple, label, ty}, rest_prime_prime_prime)
          end
      end
  end
  end

  def parse_list_type(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "[")) do
         case parse_type(Nova.Array.drop(1, ts)) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, elem_type, rest}} ->
       case expect_delimiter(rest, "]") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.ty_expr_app(Nova.Compiler.Ast.ty_expr_con("Array"), elem_type), rest_prime)
       end
   end
    else
      failure("Expected list type")
    end
  :nothing -> failure("Expected list type")
end
  end

  def parse_tuple_type(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "(")) do
         case parse_separated((&parse_type/1), fn tok -> expect_delimiter(tok, ",") end, Nova.Array.drop(1, ts)) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, elements, rest}} ->
       case expect_delimiter(rest, ")") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
case Nova.Array.length(elements) do
                        1 -> case Nova.Array.head(elements) do
                            {:just, e} -> success(e, rest_prime)
                            :nothing -> failure("Expected type")
                          end
                        _ -> success(Nova.Compiler.Ast.ty_expr_tuple(elements), rest_prime)
                      end
       end
   end
    else
      failure("Expected tuple type")
    end
  :nothing -> failure("Expected tuple type")
end
  end

  def parse_basic_type(tokens) do
    
      fold_type_app = fn base -> fn args -> Nova.Array.foldl(fn a, b -> Nova.Compiler.Ast.ty_expr_app(a, b) end, base, args) end end
      
  ts = skip_newlines(tokens)
  case Nova.Array.head(ts) do
  {:just, t} -> if (t.token_type == :tok_identifier) do
         case parse_qualified_type_name(ts) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, name, rest}} ->
       case parse_many((&parse_type_atom/1), rest) do
         {:left, err} -> {:left, err}
         {:right, {:tuple, args, rest_prime}} ->
                      base = if is_lower_case(name) do
                        Nova.Compiler.Ast.ty_expr_var(name)
                      else
                        Nova.Compiler.Ast.ty_expr_con(name)
                      end
case Nova.Array.length(args) do
                        0 -> success(base, rest_prime)
                        _ -> success(fold_type_app.(base).(args), rest_prime)
                      end
       end
   end
    else
      if ((t.token_type == :tok_delimiter) and (t.value == "(")) do
            case parse_type(Nova.Array.drop(1, ts)) do
      {:left, err} -> {:left, err}
      {:right, {:tuple, ty, rest}} ->
        case expect_delimiter(rest, ")") do
          {:left, err} -> {:left, err}
          {:right, {:tuple, _, rest_prime}} ->
success(ty, rest_prime)
        end
    end
      else
        failure("Expected basic type")
      end
    end
  :nothing -> failure("Expected basic type")
end
  end

  def parse_qualified_type_name(tokens) do
    case parse_separated((&parse_identifier_name/1), fn t -> expect_operator(t, ".") end, tokens) do
      {:left, err} -> {:left, err}
      {:right, ({:tuple, parts, rest})} -> success(Nova.String.join_with(".", parts), rest)
    end
  end

  def parse_type_atom(tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if (t.token_type == :tok_newline) do
          
            rest = Nova.Array.drop(1, tokens)
            case Nova.Array.head(rest) do
  {:just, t_prime} -> if (t_prime.token_type == :tok_newline) do
      parse_type_atom(rest)
    else
      if (t_prime.column <= 1) do
        failure("Expected type atom")
      else
        parse_type_atom(rest)
      end
    end
  _ -> parse_type_atom(rest)
end
        else
          if ((t.token_type == :tok_delimiter) and (t.value == "{")) do
            parse_record_type(tokens)
          else
            if (t.token_type == :tok_identifier) do
                     case parse_qualified_type_name(tokens) do
         {:left, err} -> {:left, err}
         {:right, {:tuple, name, rest}} ->
success(if is_lower_case(name) do
                        Nova.Compiler.Ast.ty_expr_var(name)
                      else
                        Nova.Compiler.Ast.ty_expr_con(name)
                      end, rest)
       end
            else
              if ((t.token_type == :tok_delimiter) and (t.value == "(")) do
                        case parse_type(Nova.Array.drop(1, tokens)) do
          {:left, err} -> {:left, err}
          {:right, {:tuple, ty, rest}} ->
            case expect_delimiter(rest, ")") do
              {:left, err} -> {:left, err}
              {:right, {:tuple, _, rest_prime}} ->
success(ty, rest_prime)
            end
        end
              else
                if ((t.token_type == :tok_delimiter) and (t.value == "[")) do
                           case parse_type(Nova.Array.drop(1, tokens)) do
           {:left, err} -> {:left, err}
           {:right, {:tuple, elem_type, rest}} ->
             case expect_delimiter(rest, "]") do
               {:left, err} -> {:left, err}
               {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.ty_expr_app(Nova.Compiler.Ast.ty_expr_con("Array"), elem_type), rest_prime)
             end
         end
                else
                  failure("Expected type atom")
                end
              end
            end
          end
        end
      _ -> failure("Expected type atom")
    end
  end

  def parse_pattern(tokens) do
    parse_any([(&parse_record_pattern/1), (&parse_wildcard_pattern/1), (&parse_cons_pattern/1), (&parse_constructor_pattern/1), (&parse_paren_pattern/1), (&parse_tuple_pattern/1), (&parse_list_pattern/1), (&parse_literal_pattern/1), (&parse_var_pattern/1)], tokens)
  end

  def parse_var_pattern(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (t.token_type == :tok_identifier) do
      success(Nova.Compiler.Ast.pat_var(t.value), Nova.Array.drop(1, ts))
    else
      failure("Expected variable pattern")
    end
  :nothing -> failure("Expected variable pattern")
end
  end

  def parse_wildcard_pattern(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_identifier) and (t.value == "_")) do
      success(Nova.Compiler.Ast.pat_wildcard(), Nova.Array.drop(1, ts))
    else
      failure("Expected wildcard")
    end
  :nothing -> failure("Expected wildcard")
end
  end

  def parse_literal_pattern(tokens) do
      case parse_literal(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, lit, rest}} ->
success(Nova.Compiler.Ast.pat_lit(lit), rest)
  end
  end

  def parse_constructor_pattern(tokens) do
    
      is_capital = fn s -> case Nova.String.char_at(0, s) do
        {:just, c} -> ((c >= ?A) and (c <= ?Z))
        :nothing -> false
      end end
      case parse_qualified_constructor_name(tokens) do
  {:left, err} -> {:left, err}
  {:right, {:tuple, name, rest}} ->
if is_capital.(name) do
               case parse_many((&parse_simple_pattern/1), rest) do
       {:left, err} -> {:left, err}
       {:right, {:tuple, args, rest_prime}} ->
case Nova.Array.length(args) do
                    0 -> success(Nova.Compiler.Ast.pat_con(name, []), rest_prime)
                    _ -> success(Nova.Compiler.Ast.pat_con(name, args), rest_prime)
                  end
     end
        else
          failure("Expected constructor pattern")
        end
end
  end

  def parse_qualified_constructor_name(tokens) do
    case parse_separated((&parse_identifier_name/1), fn t -> expect_operator(t, ".") end, tokens) do
      {:left, err} -> {:left, err}
      {:right, ({:tuple, parts, rest})} -> success(Nova.String.join_with(".", parts), rest)
    end
  end

  def parse_cons_pattern(tokens) do
      case parse_simple_pattern(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, hd, rest}} ->
      case expect_colon(rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, rest_prime}} ->
          case parse_pattern(rest_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, tl, rest_prime_prime}} ->
success(Nova.Compiler.Ast.pat_cons(hd, tl), rest_prime_prime)
          end
      end
  end
  end

  def parse_tuple_pattern(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "(")) do
         case parse_separated((&parse_pattern/1), fn tok -> expect_delimiter(tok, ",") end, Nova.Array.drop(1, ts)) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, elements, rest}} ->
       case expect_delimiter(rest, ")") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.pat_record(Nova.Array.map_with_index(fn i -> fn p -> {:tuple, Nova.Runtime.show(i), p} end end, elements)), rest_prime)
       end
   end
    else
      failure("Expected tuple pattern")
    end
  :nothing -> failure("Expected tuple pattern")
end
  end

  def parse_list_pattern(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "[")) do
      case Nova.Array.head(Nova.Array.drop(1, ts)) do
        {:just, t_prime} -> if ((t_prime.token_type == :tok_delimiter) and (t_prime.value == "]")) do
            success(Nova.Compiler.Ast.pat_list([]), Nova.Array.drop(2, ts))
          else
                  case parse_separated((&parse_pattern/1), fn tok -> expect_delimiter(tok, ",") end, Nova.Array.drop(1, ts)) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, elements, rest}} ->
          case expect_delimiter(rest, "]") do
            {:left, err} -> {:left, err}
            {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.pat_list(elements), rest_prime)
          end
      end
          end
        :nothing -> failure("Expected list pattern")
      end
    else
      failure("Expected list pattern")
    end
  :nothing -> failure("Expected list pattern")
end
  end

  def parse_record_pattern(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "{")) do
         case parse_separated((&parse_record_field_pattern/1), fn tok -> expect_delimiter(tok, ",") end, Nova.Array.drop(1, ts)) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, fields, rest}} ->
       case expect_delimiter(rest, "}") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.pat_record(fields), rest_prime)
       end
   end
    else
      failure("Expected record pattern")
    end
  :nothing -> failure("Expected record pattern")
end
  end

  def parse_record_field_pattern(tokens) do
      case parse_label(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, label, rest}} ->
case expect_colon(rest) do
              {:right, ({:tuple, _, rest_prime})} ->         case parse_pattern(rest_prime) do
          {:left, err} -> {:left, err}
          {:right, {:tuple, pat, rest_prime_prime}} ->
success({:tuple, label, pat}, rest_prime_prime)
        end
              {:left, _} -> case expect_operator(rest, "=") do
                  {:right, ({:tuple, _, rest_prime})} ->           case parse_pattern(rest_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, pat, rest_prime_prime}} ->
success({:tuple, label, pat}, rest_prime_prime)
          end
                  {:left, _} -> success({:tuple, label, Nova.Compiler.Ast.pat_var(label)}, rest)
                end
            end
  end
  end

  def parse_paren_pattern(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "(")) do
         case parse_pattern(Nova.Array.drop(1, ts)) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, pat, rest}} ->
       case expect_delimiter(rest, ")") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.pat_parens(pat), rest_prime)
       end
   end
    else
      failure("Expected parenthesized pattern")
    end
  :nothing -> failure("Expected parenthesized pattern")
end
  end

  def parse_simple_pattern(tokens) do
    parse_any([(&parse_literal_pattern/1), (&parse_record_pattern/1), (&parse_qualified_constructor_pattern_simple/1), (&parse_var_pattern/1), (&parse_paren_pattern/1), (&parse_tuple_pattern/1), (&parse_list_pattern/1)], tokens)
  end

  def parse_qualified_constructor_pattern_simple(tokens) do
    
      is_capital = fn s -> case Nova.String.char_at(0, s) do
        {:just, c} -> ((c >= ?A) and (c <= ?Z))
        :nothing -> false
      end end
      
  ts = skip_newlines(tokens)
  case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_identifier) and is_capital.(t.value)) do
      case Nova.Array.head(Nova.Array.drop(1, ts)) do
        {:just, t2} -> if ((t2.token_type == :tok_operator) and (t2.value == ".")) do
                  case parse_qualified_constructor_name(ts) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, name, rest}} ->
success(Nova.Compiler.Ast.pat_con(name, []), rest)
      end
          else
            success(Nova.Compiler.Ast.pat_con(t.value, []), Nova.Array.drop(1, ts))
          end
        :nothing -> success(Nova.Compiler.Ast.pat_con(t.value, []), Nova.Array.drop(1, ts))
      end
    else
      failure("Expected constructor pattern")
    end
  :nothing -> failure("Expected constructor pattern")
end
  end

  def parse_expression(tokens) do
    parse_any([(&parse_let_expression/1), (&parse_if_expression/1), (&parse_case_expression/1), (&parse_do_block/1), (&parse_lambda/1), (&parse_dollar_expression/1)], tokens)
  end

  def parse_dollar_expression(tokens) do
      case parse_hash_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
            rest_prime = skip_newlines(rest)
case Nova.Array.head(rest_prime) do
              {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "$")) do
                                    rest_prime_prime = skip_newlines(Nova.Array.drop(1, rest_prime))
         case parse_dollar_expression(rest_prime_prime) do
           {:left, err} -> {:left, err}
           {:right, {:tuple, right, rest_prime_prime_prime}} ->
success(Nova.Compiler.Ast.expr_app(left, right), rest_prime_prime_prime)
         end
                else
                  success(left, rest)
                end
              _ -> success(left, rest)
            end
  end
  end

  def parse_hash_expression(tokens) do
      case parse_logical_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
parse_hash_expression_rest(left, rest)
  end
  end

  def parse_hash_expression_rest(left, tokens) do
    
      tokens_prime = skip_newlines(tokens)
      case Nova.Array.head(tokens_prime) do
  {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "#")) do
            rest = skip_newlines(Nova.Array.drop(1, tokens_prime))
   case parse_logical_expression(rest) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, right, rest_prime}} ->
parse_hash_expression_rest(Nova.Compiler.Ast.expr_app(right, left), rest_prime)
   end
    else
      success(left, tokens)
    end
  _ -> success(left, tokens)
end
  end

  def parse_logical_expression(tokens) do
      case parse_cons_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
case Nova.Array.head(rest) do
              {:just, t} -> if ((t.token_type == :tok_operator) and ((t.value == "&&") or (t.value == "||"))) do
                           case parse_logical_expression(Nova.Array.drop(1, rest)) do
           {:left, err} -> {:left, err}
           {:right, {:tuple, right, rest_prime}} ->
success(Nova.Compiler.Ast.expr_bin_op(t.value, left, right), rest_prime)
         end
                else
                  success(left, rest)
                end
              _ -> success(left, rest)
            end
  end
  end

  def parse_cons_expression(tokens) do
      case parse_comparison_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
case Nova.Array.head(rest) do
              {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == ":")) do
                           case parse_cons_expression(Nova.Array.drop(1, rest)) do
           {:left, err} -> {:left, err}
           {:right, {:tuple, right, rest_prime}} ->
success(Nova.Compiler.Ast.expr_bin_op(":", left, right), rest_prime)
         end
                else
                  success(left, rest)
                end
              _ -> success(left, rest)
            end
  end
  end

  def parse_comparison_expression(tokens) do
    
      is_comparison_op = fn op -> ((op == "==") or ((op == "!=") or ((op == "/=") or ((op == "<") or ((op == "<=") or ((op == ">") or (op == ">="))))))) end
      tokens_prime = skip_newlines(tokens)
case parse_additive_expression(tokens_prime) do
  {:left, err} -> {:left, err}
  {:right, {:tuple, left, rest}} ->
        rest_prime = skip_newlines(rest)
case Nova.Array.head(rest_prime) do
          {:just, t} -> if ((t.token_type == :tok_operator) and is_comparison_op.(t.value)) do
                            rest_prime_prime = skip_newlines(Nova.Array.drop(1, rest_prime))
       case parse_comparison_expression(rest_prime_prime) do
         {:left, err} -> {:left, err}
         {:right, {:tuple, right, rest_prime_prime_prime}} ->
success(Nova.Compiler.Ast.expr_bin_op(t.value, left, right), rest_prime_prime_prime)
       end
            else
              success(left, rest)
            end
          _ -> success(left, rest)
        end
end
  end

  def parse_additive_expression(tokens) do
    
      is_additive_op = fn op -> ((op == "+") or ((op == "-") or ((op == "++") or (op == "<>")))) end
      case parse_multiplicative_expression(tokens) do
  {:left, err} -> {:left, err}
  {:right, {:tuple, left, rest}} ->
case Nova.Array.head(rest) do
          {:just, t} -> if ((t.token_type == :tok_operator) and is_additive_op.(t.value)) do
                     case parse_additive_expression(Nova.Array.drop(1, rest)) do
         {:left, err} -> {:left, err}
         {:right, {:tuple, right, rest_prime}} ->
success(Nova.Compiler.Ast.expr_bin_op(t.value, left, right), rest_prime)
       end
            else
              success(left, rest)
            end
          _ -> success(left, rest)
        end
end
  end

  def parse_multiplicative_expression(tokens) do
    
      is_mult_op = fn op -> ((op == "*") or (op == "/")) end
      case parse_backtick_expression(tokens) do
  {:left, err} -> {:left, err}
  {:right, {:tuple, left, rest}} ->
case Nova.Array.head(rest) do
          {:just, t} -> if ((t.token_type == :tok_operator) and is_mult_op.(t.value)) do
                     case parse_multiplicative_expression(Nova.Array.drop(1, rest)) do
         {:left, err} -> {:left, err}
         {:right, {:tuple, right, rest_prime}} ->
success(Nova.Compiler.Ast.expr_bin_op(t.value, left, right), rest_prime)
       end
            else
              success(left, rest)
            end
          _ -> success(left, rest)
        end
end
  end

  def parse_backtick_expression(tokens) do
    
      parse_backtick_qualified = Nova.Runtime.fix2(fn parse_backtick_qualified -> fn name -> fn toks -> case Nova.Array.head(toks) do
        {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == ".")) do
            case Nova.Array.head(Nova.Array.drop(1, toks)) do
              {:just, t_prime} -> if (t_prime.token_type == :tok_identifier) do
                  parse_backtick_qualified.(Nova.Runtime.append(name, Nova.Runtime.append(".", t_prime.value))).(Nova.Array.drop(2, toks))
                else
                  success(Nova.Compiler.Ast.expr_var(name), toks)
                end
              _ -> success(Nova.Compiler.Ast.expr_var(name), toks)
            end
          else
            success(Nova.Compiler.Ast.expr_var(name), toks)
          end
        _ -> success(Nova.Compiler.Ast.expr_var(name), toks)
      end  end end end)
      parse_backtick_fn = fn toks -> case Nova.Array.head(toks) do
        {:just, t} -> if (t.token_type == :tok_identifier) do
            parse_backtick_qualified.(t.value).(Nova.Array.drop(1, toks))
          else
            failure("Expected identifier in backtick expression")
          end
        _ -> failure("Expected identifier in backtick expression")
      end end
      parse_backtick_rest = Nova.Runtime.fix2(fn parse_backtick_rest -> fn left -> fn rest -> case Nova.Array.head(rest) do
        {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "`")) do
                  case parse_backtick_fn.(Nova.Array.drop(1, rest)) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, fn_, rest_prime}} ->
case Nova.Array.head(rest_prime) do
                      {:just, t_prime} -> if ((t_prime.token_type == :tok_operator) and (t_prime.value == "`")) do
                                       case parse_unary_expression(Nova.Array.drop(1, rest_prime)) do
               {:left, err} -> {:left, err}
               {:right, {:tuple, right, rest_prime_prime}} ->
                                  result = Nova.Compiler.Ast.expr_app(Nova.Compiler.Ast.expr_app(fn_, left), right)
parse_backtick_rest.(result).(rest_prime_prime)
             end
                        else
                          failure("Expected closing backtick")
                        end
                      _ -> failure("Expected closing backtick")
                    end
      end
          else
            success(left, rest)
          end
        _ -> success(left, rest)
      end  end end end)
      case parse_unary_expression(tokens) do
  {:left, err} -> {:left, err}
  {:right, {:tuple, left, rest}} ->
parse_backtick_rest.(left).(rest)
end
  end

  def parse_unary_expression(tokens) do
    
      is_unary_op = fn op -> ((op == "-") or ((op == "+") or (op == "!"))) end
      case Nova.Array.head(tokens) do
  {:just, t} -> if ((t.token_type == :tok_operator) and is_unary_op.(t.value)) do
         case parse_unary_expression(Nova.Array.drop(1, tokens)) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, expr, rest}} ->
success(Nova.Compiler.Ast.expr_unary_op(t.value, expr), rest)
   end
    else
      parse_application(tokens)
    end
  _ -> parse_application(tokens)
end
  end

  def parse_application(tokens) do
    
      fold_app = fn fn_ -> fn args -> Nova.Array.foldl(fn a, b -> Nova.Compiler.Ast.expr_app(a, b) end, fn_, args) end end
      case Nova.Array.head(tokens) do
  {:just, first_tok} ->   case parse_term(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, fn_, rest}} ->
      case maybe_parse_record_update(fn_, rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, fn_prime, rest_prime}} ->
                    {:tuple, args, rest_prime_prime} = collect_application_args(rest_prime, [], first_tok.column)
case Nova.Array.length(args) do
                      0 -> success(fn_prime, rest_prime_prime)
                      _ -> success(fold_app.(fn_prime).(args), rest_prime_prime)
                    end
      end
  end
  :nothing -> failure("No tokens remaining")
end
  end

  def maybe_parse_record_update(expr, tokens) do
    
      is_record_update = fn toks -> case Nova.Array.head(toks) do
        {:just, t1} -> if (t1.token_type == :tok_identifier) do
            case Nova.Array.head(Nova.Array.drop(1, toks)) do
              {:just, t2} -> if ((t2.token_type == :tok_operator) and (t2.value == "=")) do
                  true
                else
                  false
                end
              _ -> false
            end
          else
            false
          end
        _ -> false
      end end
      case Nova.Array.head(tokens) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "{")) do
      case is_record_update.(Nova.Array.drop(1, tokens)) do
        true ->      case parse_record_update_fields(Nova.Array.drop(1, tokens)) do
       {:left, err} -> {:left, err}
       {:right, {:tuple, updates, rest}} ->
         case expect_delimiter(rest, "}") do
           {:left, err} -> {:left, err}
           {:right, {:tuple, _, rest_prime}} ->
maybe_parse_record_update(Nova.Compiler.Ast.expr_record_update(expr, updates), rest_prime)
         end
     end
        false -> success(expr, tokens)
      end
    else
      success(expr, tokens)
    end
  _ -> success(expr, tokens)
end
  end

  def parse_record_update_fields(tokens) do
    parse_separated((&parse_record_update_field/1), fn t -> expect_delimiter(t, ",") end, tokens)
  end

  def parse_record_update_field(tokens) do
      case parse_identifier_name(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, label, rest}} ->
      case expect_operator(rest, "=") do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, rest_prime}} ->
          case parse_expression(rest_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, expr, rest_prime_prime}} ->
success({:tuple, label, expr}, rest_prime_prime)
          end
      end
  end
  end

  def collect_application_args(tokens, acc, base) do
    
      looks_like_record_binding = fn toks -> 
        go = Nova.Runtime.fix2(fn go -> fn toks_prime -> fn depth -> case Nova.Array.head(toks_prime) do
          :nothing -> false
          {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "{")) do
              go.(Nova.Array.drop(1, toks_prime)).((depth + 1))
            else
              if ((t.token_type == :tok_delimiter) and (t.value == "}")) do
                if (depth == 1) do
                  case Nova.Array.head(skip_newlines(Nova.Array.drop(1, toks_prime))) do
                    {:just, t2} -> if ((t2.token_type == :tok_operator) and (t2.value == "=")) do
                        true
                      else
                        false
                      end
                    _ -> false
                  end
                else
                  go.(Nova.Array.drop(1, toks_prime)).((depth - 1))
                end
              else
                go.(Nova.Array.drop(1, toks_prime)).(depth)
              end
            end
        end  end end end)
        go.(toks).(1) end
      is_continuation_token = fn t -> ((t.token_type == :tok_delimiter) and ((t.value == "[") or ((t.value == "(") or (t.value == "{")))) end
      looks_like_binding = fn toks -> case Nova.Array.head(toks) do
        {:just, t1} -> if (t1.token_type == :tok_identifier) do
            case Nova.Array.head(skip_newlines(Nova.Array.drop(1, toks))) do
              {:just, t2} -> if ((t2.token_type == :tok_operator) and (t2.value == "=")) do
                  true
                else
                  false
                end
              _ -> false
            end
          else
            if ((t1.token_type == :tok_delimiter) and (t1.value == "{")) do
              looks_like_record_binding.(Nova.Array.drop(1, toks))
            else
              false
            end
          end
        _ -> false
      end end
      case Nova.Array.head(tokens) do
  {:just, t} -> if (t.token_type == :tok_newline) do
      
        rest = skip_newlines(Nova.Array.drop(1, tokens))
        case Nova.Array.head(rest) do
  {:just, t_prime} -> if looks_like_binding.(rest) do
      {:tuple, acc, rest}
    else
      if ((t_prime.column > 1) and is_continuation_token.(t_prime)) do
        case parse_term(rest) do
          {:right, ({:tuple, arg, rest_prime})} -> collect_application_args(rest_prime, Nova.Array.snoc(acc, arg), base)
          {:left, _} -> {:tuple, acc, rest}
        end
      else
        if ((t_prime.column > 1) and (t_prime.column > base)) do
          case parse_term(rest) do
            {:right, ({:tuple, arg, rest_prime})} -> collect_application_args(rest_prime, Nova.Array.snoc(acc, arg), base)
            {:left, _} -> {:tuple, acc, rest}
          end
        else
          {:tuple, acc, rest}
        end
      end
    end
  _ -> {:tuple, acc, rest}
end
    else
      case parse_term(tokens) do
        {:right, ({:tuple, arg, rest})} -> collect_application_args(rest, Nova.Array.snoc(acc, arg), base)
        {:left, _} -> {:tuple, acc, tokens}
      end
    end
  _ -> case parse_term(tokens) do
      {:right, ({:tuple, arg, rest})} -> collect_application_args(rest, Nova.Array.snoc(acc, arg), base)
      {:left, _} -> {:tuple, acc, tokens}
    end
end
  end

  def parse_term(tokens) do
    parse_any([(&parse_record_literal/1), (&parse_expr_literal/1), (&parse_list_literal/1), (&parse_tuple_literal/1), (&parse_qualified_identifier/1), (&parse_paren_expr/1)], tokens)
  end

  def parse_expr_literal(tokens) do
      case parse_literal(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, lit, rest}} ->
success(Nova.Compiler.Ast.expr_lit(lit), rest)
  end
  end

  def parse_paren_expr(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "(")) do
         case parse_expression(Nova.Array.drop(1, ts)) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, expr, rest}} ->
       case expect_delimiter(rest, ")") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.expr_parens(expr), rest_prime)
       end
   end
    else
      failure("Expected parenthesized expression")
    end
  :nothing -> failure("Expected parenthesized expression")
end
  end

  def parse_record_literal(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "{")) do
         case parse_separated((&parse_record_field_expr/1), fn tok -> expect_delimiter(tok, ",") end, Nova.Array.drop(1, ts)) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, fields, rest}} ->
       case expect_delimiter(rest, "}") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.expr_record(fields), rest_prime)
       end
   end
    else
      failure("Expected record literal")
    end
  :nothing -> failure("Expected record literal")
end
  end

  def parse_record_field_expr(tokens) do
      case parse_identifier_name(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, label, rest}} ->
case expect_colon(rest) do
              {:right, ({:tuple, _, rest_prime})} ->                 rest_prime_prime = skip_newlines(rest_prime)
        case parse_expression(rest_prime_prime) do
          {:left, err} -> {:left, err}
          {:right, {:tuple, expr, rest_prime_prime_prime}} ->
success({:tuple, label, expr}, rest_prime_prime_prime)
        end
              {:left, _} -> success({:tuple, label, Nova.Compiler.Ast.expr_var(label)}, rest)
            end
  end
  end

  def parse_list_literal(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "[")) do
      case Nova.Array.head(Nova.Array.drop(1, ts)) do
        {:just, t_prime} -> if ((t_prime.token_type == :tok_delimiter) and (t_prime.value == "]")) do
            success(Nova.Compiler.Ast.expr_list([]), Nova.Array.drop(2, ts))
          else
                  case parse_separated((&parse_expression/1), fn tok -> expect_delimiter(tok, ",") end, Nova.Array.drop(1, ts)) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, elements, rest}} ->
          case expect_delimiter(rest, "]") do
            {:left, err} -> {:left, err}
            {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.expr_list(elements), rest_prime)
          end
      end
          end
        :nothing -> failure("Expected list literal")
      end
    else
      failure("Expected list literal")
    end
  :nothing -> failure("Expected list literal")
end
  end

  def parse_tuple_literal(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "(")) do
         case parse_separated((&parse_expression/1), fn tok -> expect_delimiter(tok, ",") end, Nova.Array.drop(1, ts)) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, elements, rest}} ->
       case expect_delimiter(rest, ")") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
case Nova.Array.length(elements) do
                        1 -> case Nova.Array.head(elements) do
                            {:just, e} -> success(e, rest_prime)
                            :nothing -> failure("Expected expression")
                          end
                        _ -> success(Nova.Compiler.Ast.expr_tuple(elements), rest_prime)
                      end
       end
   end
    else
      failure("Expected tuple literal")
    end
  :nothing -> failure("Expected tuple literal")
end
  end

  def parse_let_expression(tokens) do
        tokens_prime = skip_newlines(tokens)
  case expect_keyword(tokens_prime, "let") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
            rest_prime = skip_newlines(rest)
      case parse_many((&parse_binding/1), rest_prime) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, bindings, rest_prime_prime}} ->
                    rest_prime_prime_prime = skip_newlines(rest_prime_prime)
          case expect_keyword(rest_prime_prime_prime, "in") do
            {:left, err} -> {:left, err}
            {:right, {:tuple, _, rest4}} ->
                            rest5 = skip_newlines(rest4)
              case parse_expression(rest5) do
                {:left, err} -> {:left, err}
                {:right, {:tuple, body, rest6}} ->
success(Nova.Compiler.Ast.expr_let(bindings, body), rest6)
              end
          end
      end
  end
  end

  def parse_binding(tokens) do
        tokens_prime = skip_newlines(tokens)
case parse_function_style_binding(tokens_prime) do
      {:right, r} -> {:right, r}
      {:left, _} ->     case parse_pattern(tokens_prime) do
      {:left, err} -> {:left, err}
      {:right, {:tuple, pat, rest}} ->
        case expect_operator(rest, "=") do
          {:left, err} -> {:left, err}
          {:right, {:tuple, _, rest_prime}} ->
            case parse_expression(rest_prime) do
              {:left, err} -> {:left, err}
              {:right, {:tuple, expr, rest_prime_prime}} ->
success(%{pattern: pat, value: expr, type_ann: :nothing}, rest_prime_prime)
            end
        end
    end
    end
  end

  def parse_function_style_binding(tokens) do
    
      is_upper_case = fn s -> case Nova.String.char_at(0, s) do
        {:just, c} -> ((c >= ?A) and (c <= ?Z))
        :nothing -> false
      end end
      collect_params = Nova.Runtime.fix2(fn collect_params -> fn toks -> fn acc -> 
        toks_prime = skip_newlines(toks)
        case Nova.Array.head(toks_prime) do
  {:just, tok} -> if ((tok.token_type == :tok_operator) and (tok.value == "=")) do
      {:tuple, Nova.Array.reverse(acc), toks_prime}
    else
      case parse_simple_pattern(toks_prime) do
        {:right, ({:tuple, pat, rest})} -> collect_params.(rest).(Nova.Array.cons(pat, acc))
        {:left, _} -> {:tuple, Nova.Array.reverse(acc), toks_prime}
      end
    end
  _ -> case parse_simple_pattern(toks_prime) do
      {:right, ({:tuple, pat, rest})} -> collect_params.(rest).(Nova.Array.cons(pat, acc))
      {:left, _} -> {:tuple, Nova.Array.reverse(acc), toks_prime}
    end
end  end end end)
      case Nova.Array.head(tokens) do
  {:just, t} -> if ((t.token_type == :tok_identifier) and not(is_upper_case.(t.value))) do
      
        name = t.value
        rest = Nova.Array.drop(1, tokens)
        {:tuple, params, rest_prime} = collect_params.(rest).([])
        case Nova.Array.length(params) do
  0 -> failure("Not a function binding")
  _ -> case expect_operator(rest_prime, "=") do
      {:left, err} -> {:left, err}
      {:right, ({:tuple, _, rest_prime_prime})} -> case parse_expression(rest_prime_prime) do
          {:left, err} -> {:left, err}
          {:right, ({:tuple, body, rest_prime_prime_prime})} -> 
              lambda = Nova.Compiler.Ast.expr_lambda(params, body)
              success(%{pattern: Nova.Compiler.Ast.pat_var(name), value: lambda, type_ann: :nothing}, rest_prime_prime_prime)
        end
    end
end
    else
      failure("Not a function binding")
    end
  _ -> failure("Not a function binding")
end
  end

  def parse_if_expression(tokens) do
        tokens_prime = skip_newlines(tokens)
  case expect_keyword(tokens_prime, "if") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
      case parse_expression(rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, cond_, rest_prime}} ->
          case expect_keyword(rest_prime, "then") do
            {:left, err} -> {:left, err}
            {:right, {:tuple, _, rest_prime_prime}} ->
              case parse_expression(rest_prime_prime) do
                {:left, err} -> {:left, err}
                {:right, {:tuple, then_branch, rest_prime_prime_prime}} ->
                  case expect_keyword(rest_prime_prime_prime, "else") do
                    {:left, err} -> {:left, err}
                    {:right, {:tuple, _, rest4}} ->
                      case parse_expression(rest4) do
                        {:left, err} -> {:left, err}
                        {:right, {:tuple, else_branch, rest5}} ->
success(Nova.Compiler.Ast.expr_if(cond_, then_branch, else_branch), rest5)
                      end
                  end
              end
          end
      end
  end
  end

  def parse_case_expression(tokens) do
      case expect_keyword(tokens, "case") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
      case parse_expression(rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, expr, rest_prime}} ->
                    rest_prime_prime = skip_newlines(rest_prime)
          case expect_keyword(rest_prime_prime, "of") do
            {:left, err} -> {:left, err}
            {:right, {:tuple, _, rest_prime_prime_prime}} ->
                            rest4 = skip_newlines(rest_prime_prime_prime)
case Nova.Array.head(rest4) do
                              :nothing -> failure("Expected case clauses")
                              {:just, first_tok} ->                 case parse_case_clauses_at(rest4, first_tok.column, []) do
                  {:left, err} -> {:left, err}
                  {:right, {:tuple, clauses, rest5}} ->
success(Nova.Compiler.Ast.expr_case(expr, clauses), rest5)
                end
                            end
          end
      end
  end
  end

  def parse_case_clauses_at(tokens, indent, acc) do
    
      parse_additional_guard = Nova.Runtime.fix4(fn parse_additional_guard -> fn toks -> fn pat -> fn clause_indent -> fn clause_acc -> 
        toks_prime = skip_newlines(toks)
        case Nova.Array.head(toks_prime) do
  {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "|")) do
      case parse_guard_expression(Nova.Array.drop(1, toks_prime)) do
        {:right, ({:tuple, guard, after_guard})} ->      case expect_operator(after_guard, "->") do
       {:left, err} -> {:left, err}
       {:right, {:tuple, _, after_arrow}} ->
case Nova.Array.head(skip_newlines(after_arrow)) do
                    {:just, first_body_tok} ->                       {:tuple, body_tokens, rest} = take_body(after_arrow, [], first_body_tok.column)
           case parse_expression(body_tokens) do
             {:left, err} -> {:left, err}
             {:right, {:tuple, body, remaining}} ->
                              clause = %{pattern: pat, guard: {:just, guard}, body: body}
                              new_acc = Nova.Array.snoc(clause_acc, clause)
                              rest_after_body = skip_newlines(rest)
case Nova.Array.head(rest_after_body) do
                                {:just, t_prime} -> if ((t_prime.token_type == :tok_operator) and (t_prime.value == "|")) do
                                    parse_additional_guard.(rest_after_body).(pat).(clause_indent).(new_acc)
                                  else
                                    parse_case_clauses_at(rest, clause_indent, new_acc)
                                  end
                                _ -> parse_case_clauses_at(rest, clause_indent, new_acc)
                              end
           end
                    :nothing -> failure("Expected body after ->")
                  end
     end
        {:left, err} -> {:left, err}
      end
    else
      parse_case_clauses_at(toks, clause_indent, clause_acc)
    end
  _ -> parse_case_clauses_at(toks, clause_indent, clause_acc)
end  end end end end end)
      
  tokens_prime = skip_newlines(tokens)
  case Nova.Array.head(tokens_prime) do
  :nothing -> if (Nova.Array.length(acc) > 0) do
      success(acc, tokens_prime)
    else
      failure("Expected case clause")
    end
  {:just, t} -> if ((t.token_type == :tok_operator) and ((t.value == "|") and (Nova.Array.length(acc) > 0))) do
      case Nova.Array.last(acc) do
        {:just, prev_clause} -> parse_additional_guard.(tokens_prime).(prev_clause.pattern).(indent).(acc)
        :nothing -> failure("Internal error: no previous clause")
      end
    else
      if ((t.column != indent) and (Nova.Array.length(acc) > 0)) do
        success(acc, tokens_prime)
      else
        if (t.column != indent) do
          failure("Case clause at wrong indentation")
        else
          case parse_case_clause(tokens_prime) do
            {:right, ({:tuple, clause, rest})} -> 
                rest_prime = skip_newlines(rest)
                case Nova.Array.head(rest_prime) do
  {:just, t2} -> if ((t2.token_type == :tok_operator) and (t2.value == "|")) do
      parse_additional_guard.(rest_prime).(clause.pattern).(indent).(Nova.Array.snoc(acc, clause))
    else
      parse_case_clauses_at(rest, indent, Nova.Array.snoc(acc, clause))
    end
  _ -> parse_case_clauses_at(rest, indent, Nova.Array.snoc(acc, clause))
end
            {:left, _} -> if (Nova.Array.length(acc) > 0) do
                success(acc, tokens_prime)
              else
                failure("Expected case clause")
              end
          end
        end
      end
    end
end
  end

  def parse_case_clause(tokens) do
    
      has_more_guards = fn toks -> case Nova.Array.head(skip_newlines(toks)) do
        {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "|")) do
            true
          else
            false
          end
        _ -> false
      end end
      tokens_prime = skip_newlines(tokens)
case Nova.Array.head(tokens_prime) do
  :nothing -> failure("No more tokens to parse")
  {:just, first_tok} ->   case parse_pattern(tokens_prime) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, pat, rest}} ->
            {:tuple, guard, rest_prime} = maybe_parse_guard(rest)
      case expect_operator(rest_prime, "->") do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, rest_prime_prime}} ->
                    {:tuple, body_tokens, rest_prime_prime_prime} = take_body(rest_prime_prime, [], first_tok.column)
          case parse_expression(body_tokens) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, body, remaining}} ->
case skip_newlines(remaining) do
                              [] -> success(%{pattern: pat, guard: guard, body: body}, drop_newlines(rest_prime_prime_prime))
                              _ -> case has_more_guards.(remaining) do
                                  true -> success(%{pattern: pat, guard: guard, body: body}, Nova.Runtime.append(remaining, rest_prime_prime_prime))
                                  false -> failure("Unexpected tokens after case-clause body")
                                end
                            end
          end
      end
  end
end
  end

  def maybe_parse_guard(tokens) do
    
      tokens_prime = skip_newlines(tokens)
      case Nova.Array.head(tokens_prime) do
  {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "|")) do
      case parse_guard_expression(Nova.Array.drop(1, tokens_prime)) do
        {:right, ({:tuple, guard, rest})} -> {:tuple, {:just, guard}, rest}
        {:left, _} -> {:tuple, :nothing, tokens}
      end
    else
      {:tuple, :nothing, tokens}
    end
  _ -> {:tuple, :nothing, tokens}
end
  end

  def parse_guard_expression(tokens) do
    
      fold_guards = fn gs -> case Nova.Array.uncons(gs) do
        {:just, %{head: head, tail: tail}} -> Nova.Array.foldl(fn acc -> fn g -> Nova.Compiler.Ast.expr_bin_op("&&", acc, g) end end, head, tail)
        :nothing -> Nova.Compiler.Ast.expr_lit(Nova.Compiler.Ast.lit_bool(true))
      end end
      case parse_guard_parts(tokens, []) do
  {:left, err} -> {:left, err}
  {:right, {:tuple, guards, rest}} ->
case Nova.Array.length(guards) do
          0 -> failure("Expected guard expression")
          1 -> case Nova.Array.head(guards) do
              {:just, g} -> success(g, rest)
              :nothing -> failure("No guard")
            end
          _ -> success(fold_guards.(guards), rest)
        end
end
  end

  def parse_guard_parts(tokens, acc) do
      case parse_guard_part(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, g, rest}} ->
            rest_prime = skip_newlines(rest)
case Nova.Array.head(rest_prime) do
              {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == ",")) do
                  parse_guard_parts(Nova.Array.drop(1, rest_prime), Nova.Array.snoc(acc, g))
                else
                  success(Nova.Array.snoc(acc, g), rest_prime)
                end
              _ -> success(Nova.Array.snoc(acc, g), rest_prime)
            end
  end
  end

  def parse_guard_part(tokens) do
    
      pattern_to_expr = Nova.Runtime.fix(fn pattern_to_expr -> fn __arg0__ -> case __arg0__ do
        ({:pat_var, v}) -> Nova.Compiler.Ast.expr_var(v)
        ({:pat_con, c, args}) -> Nova.Runtime.foldl(fn a, b -> Nova.Compiler.Ast.expr_app(a, b) end, Nova.Compiler.Ast.expr_var(c), Nova.Runtime.map(pattern_to_expr, args))
        ({:pat_lit, l}) -> Nova.Compiler.Ast.expr_lit(l)
        :pat_wildcard -> Nova.Compiler.Ast.expr_var("_")
        ({:pat_record, fields}) -> Nova.Compiler.Ast.expr_record(Nova.Runtime.map(fn ({:tuple, k, p}) -> {:tuple, k, pattern_to_expr.(p)} end, fields))
        ({:pat_cons, h, t}) -> Nova.Compiler.Ast.expr_bin_op(":", pattern_to_expr.(h), pattern_to_expr.(t))
        ({:pat_as, n, p}) -> pattern_to_expr.(p)
        ({:pat_list, ps}) -> Nova.Compiler.Ast.expr_list(Nova.Runtime.map(pattern_to_expr, ps))
        ({:pat_parens, p}) -> Nova.Compiler.Ast.expr_parens(pattern_to_expr.(p))
      end end end)
      try_pattern_bind = fn toks ->    case parse_pattern(toks) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, pat, rest}} ->
              rest_prime = skip_newlines(rest)
case Nova.Array.head(rest_prime) do
                {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "<-")) do
                              case parse_logical_expression(Nova.Array.drop(1, rest_prime)) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, expr, rest_prime_prime}} ->
success(Nova.Compiler.Ast.expr_bin_op("<-", pattern_to_expr.(pat), expr), rest_prime_prime)
          end
                  else
                    failure("Not a pattern bind")
                  end
                _ -> failure("Not a pattern bind")
              end
   end end
      tokens_prime = skip_newlines(tokens)
case try_pattern_bind.(tokens_prime) do
  {:right, result} -> {:right, result}
  {:left, _} -> parse_logical_expression(tokens_prime)
end
  end

  def take_body(tokens, acc, indent) do
    case Nova.Array.head(tokens) do
      :nothing -> {:tuple, Nova.Array.reverse(acc), []}
      {:just, t} -> if (t.token_type == :tok_newline) do
          
            rest = skip_newlines(Nova.Array.drop(1, tokens))
            case Nova.Array.head(rest) do
  {:just, t_prime} -> if (t_prime.column < indent) do
      {:tuple, Nova.Array.reverse(acc), rest}
    else
      if ((t_prime.column == indent) and clause_start(rest)) do
        {:tuple, Nova.Array.reverse(acc), rest}
      else
        if ((t_prime.token_type == :tok_operator) and ((t_prime.value == "|") and guard_start(rest))) do
          {:tuple, Nova.Array.reverse(acc), rest}
        else
          take_body(rest, Nova.Array.cons(t, acc), indent)
        end
      end
    end
  _ -> take_body(rest, Nova.Array.cons(t, acc), indent)
end
        else
          take_body(Nova.Array.drop(1, tokens), Nova.Array.cons(t, acc), indent)
        end
    end
  end

  def guard_start(tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "|")) do
          case parse_guard_expression(Nova.Array.drop(1, tokens)) do
            {:right, ({:tuple, _, rest})} -> case expect_operator(rest, "->") do
                {:right, _} -> true
                {:left, _} -> false
              end
            {:left, _} -> false
          end
        else
          false
        end
      _ -> false
    end
  end

  def clause_start(tokens) do
    
      maybe_parse_guard_indented = fn pat_col -> fn toks -> case Nova.Array.head(toks) do
        {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "|")) do
            case parse_guard_expression(Nova.Array.drop(1, toks)) do
              {:right, ({:tuple, guard, rest})} -> {:tuple, {:just, guard}, rest}
              {:left, _} -> {:tuple, :nothing, toks}
            end
          else
            if (t.token_type == :tok_newline) do
              
                toks_prime = skip_newlines(toks)
                case Nova.Array.head(toks_prime) do
  {:just, t_prime} -> if ((t_prime.token_type == :tok_operator) and ((t_prime.value == "|") and (t_prime.column > pat_col))) do
      case parse_guard_expression(Nova.Array.drop(1, toks_prime)) do
        {:right, ({:tuple, guard, rest})} -> {:tuple, {:just, guard}, rest}
        {:left, _} -> {:tuple, :nothing, toks}
      end
    else
      {:tuple, :nothing, toks}
    end
  _ -> {:tuple, :nothing, toks}
end
            else
              {:tuple, :nothing, toks}
            end
          end
        _ -> {:tuple, :nothing, toks}
      end end end
      case parse_pattern(tokens) do
  {:right, ({:tuple, _, rest})} -> case Nova.Array.head(tokens) do
      {:just, pat_tok} -> 
          {:tuple, _, rest_prime} = maybe_parse_guard_indented.(pat_tok.column).(rest)
          case expect_operator(rest_prime, "->") do
  {:right, _} -> true
  {:left, _} -> false
end
      :nothing -> false
    end
  {:left, _} -> false
end
  end

  def parse_do_block(tokens) do
      case expect_keyword(tokens, "do") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
            rest_prime = skip_newlines(rest)
case Nova.Array.head(rest_prime) do
              :nothing -> success(Nova.Compiler.Ast.expr_do([]), rest_prime)
              {:just, first_tok} ->                 indent = first_tok.column
        case parse_do_statements_at(rest_prime, indent, []) do
          {:left, err} -> {:left, err}
          {:right, {:tuple, stmts, rest_prime_prime}} ->
success(Nova.Compiler.Ast.expr_do(stmts), rest_prime_prime)
        end
            end
  end
  end

  def parse_do_statements_at(tokens, indent, acc) do
    
      tokens_prime = skip_newlines(tokens)
      case Nova.Array.head(tokens_prime) do
  :nothing -> success(acc, tokens_prime)
  {:just, t} -> if (t.column < indent) do
      success(acc, tokens_prime)
    else
      if ((t.column > indent) and (Nova.Array.length(acc) == 0)) do
        case parse_do_statement(tokens_prime) do
          {:right, ({:tuple, stmt, rest})} -> parse_do_statements_at(rest, indent, Nova.Array.snoc(acc, stmt))
          {:left, _} -> success(acc, tokens_prime)
        end
      else
        if ((t.column != indent) and (Nova.Array.length(acc) > 0)) do
          success(acc, tokens_prime)
        else
          case parse_do_statement(tokens_prime) do
            {:right, ({:tuple, stmt, rest})} -> parse_do_statements_at(rest, indent, Nova.Array.snoc(acc, stmt))
            {:left, _} -> if (Nova.Array.length(acc) > 0) do
                success(acc, tokens_prime)
              else
                failure("Expected do statement")
              end
          end
        end
      end
    end
end
  end

  def parse_do_statement(tokens) do
    parse_any([(&parse_do_let/1), (&parse_do_bind/1), (&parse_do_expr/1)], tokens)
  end

  def parse_do_let(tokens) do
      case expect_keyword(tokens, "let") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
      case parse_many((&parse_binding/1), rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, bindings, rest_prime}} ->
success(Nova.Compiler.Ast.do_let(bindings), rest_prime)
      end
  end
  end

  def parse_do_bind(tokens) do
      case parse_pattern(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, pat, rest}} ->
      case expect_operator(rest, "<-") do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, rest_prime}} ->
          case parse_expression(rest_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, expr, rest_prime_prime}} ->
success(Nova.Compiler.Ast.do_bind(pat, expr), rest_prime_prime)
          end
      end
  end
  end

  def parse_do_expr(tokens) do
      case parse_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, expr, rest}} ->
success(Nova.Compiler.Ast.do_expr(expr), rest)
  end
  end

  def parse_lambda(tokens) do
      case expect_operator(tokens, "\\") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
      case parse_many((&parse_simple_pattern/1), rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, params, rest_prime}} ->
          case expect_operator(rest_prime, "->") do
            {:left, err} -> {:left, err}
            {:right, {:tuple, _, rest_prime_prime}} ->
              case parse_expression(rest_prime_prime) do
                {:left, err} -> {:left, err}
                {:right, {:tuple, body, rest_prime_prime_prime}} ->
success(Nova.Compiler.Ast.expr_lambda(params, body), rest_prime_prime_prime)
              end
          end
      end
  end
  end

  def parse_type_signature(tokens) do
        tokens_prime = drop_newlines(tokens)
  case parse_identifier_name(tokens_prime) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, name, rest}} ->
      case expect_operator(rest, "::") do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, rest_prime}} ->
          case parse_type(rest_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, ty, rest_prime_prime}} ->
success(%{name: name, type_vars: [], constraints: [], ty: ty}, rest_prime_prime)
          end
      end
  end
  end

  def parse_declaration(tokens) do
    parse_any([(&parse_module_header/1), (&parse_import/1), (&parse_foreign_import_simple/1), (&parse_data_declaration/1), (&parse_type_alias/1), (&parse_type_class/1), (&parse_type_class_instance/1), (&parse_function_with_type_signature/1), (&parse_function_declaration/1), (&parse_type_signature_decl/1)], tokens)
  end

  def parse_module_header(tokens) do
        tokens_prime = drop_newlines(tokens)
  case expect_keyword(tokens_prime, "module") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
      case parse_qualified_identifier_name(rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, name, rest_prime}} ->
          case expect_keyword(rest_prime, "where") do
            {:left, err} -> {:left, err}
            {:right, {:tuple, _, rest_prime_prime}} ->
success(Nova.Compiler.Ast.decl_module(%{name: name, declarations: []}), rest_prime_prime)
          end
      end
  end
  end

  def parse_qualified_identifier_name(tokens) do
    case parse_separated((&parse_identifier_name/1), fn t -> expect_operator(t, ".") end, tokens) do
      {:left, err} -> {:left, err}
      {:right, ({:tuple, parts, rest})} -> success(Nova.String.join_with(".", parts), rest)
    end
  end

  def parse_import(tokens) do
        tokens_prime = drop_newlines(tokens)
  case expect_keyword(tokens_prime, "import") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
      case parse_qualified_identifier_name(rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, mod_name, rest_prime}} ->
                    {:tuple, alias_, rest_prime_prime} = parse_import_alias(rest_prime)
          case parse_import_selectors(rest_prime_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, result, rest_prime_prime_prime}} ->
                            {:tuple, items, hiding} = result
success(Nova.Compiler.Ast.decl_import(%{module_name: mod_name, alias_: alias_, items: items, hiding: hiding}), drop_newlines(rest_prime_prime_prime))
          end
      end
  end
  end

  def parse_import_alias(tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if ((t.token_type == :tok_identifier) and (t.value == "as")) do
          case parse_identifier_name(Nova.Array.drop(1, tokens)) do
            {:right, ({:tuple, name, rest})} -> {:tuple, {:just, name}, rest}
            {:left, _} -> {:tuple, :nothing, tokens}
          end
        else
          {:tuple, :nothing, tokens}
        end
      _ -> {:tuple, :nothing, tokens}
    end
  end

  def parse_import_selectors(tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if ((t.token_type == :tok_identifier) and (t.value == "hiding")) do
          case parse_paren_import_list(Nova.Array.drop(1, tokens)) do
            {:right, ({:tuple, items, rest})} -> success({:tuple, items, true}, rest)
            {:left, err} -> {:left, err}
          end
        else
          case parse_paren_import_list(tokens) do
            {:right, ({:tuple, items, rest})} -> success({:tuple, items, false}, rest)
            {:left, _} -> success({:tuple, [], false}, tokens)
          end
        end
      _ -> case parse_paren_import_list(tokens) do
          {:right, ({:tuple, items, rest})} -> success({:tuple, items, false}, rest)
          {:left, _} -> success({:tuple, [], false}, tokens)
        end
    end
  end

  def parse_paren_import_list(tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "(")) do
               case parse_separated((&parse_import_item/1), fn tok -> expect_delimiter(tok, ",") end, Nova.Array.drop(1, tokens)) do
       {:left, err} -> {:left, err}
       {:right, {:tuple, items, rest}} ->
         case expect_delimiter(rest, ")") do
           {:left, err} -> {:left, err}
           {:right, {:tuple, _, rest_prime}} ->
success(items, rest_prime)
         end
     end
        else
          failure("No paren import list")
        end
      _ -> failure("No paren import list")
    end
  end

  def parse_import_item(tokens) do
    
      parse_normal_import_item = fn toks ->    case parse_identifier_name(toks) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, name, rest}} ->
case Nova.Array.head(rest) do
                {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "(")) do
                              case parse_import_spec(Nova.Array.drop(1, rest)) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, spec, rest_prime}} ->
success(Nova.Compiler.Ast.import_type(name, spec), rest_prime)
          end
                  else
                    success(Nova.Compiler.Ast.import_value(name), rest)
                  end
                _ -> success(Nova.Compiler.Ast.import_value(name), rest)
              end
   end end
      case Nova.Array.head(tokens) do
  {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "(")) do
      case Nova.Array.head(Nova.Array.drop(1, tokens)) do
        {:just, op_tok} -> if (op_tok.token_type == :tok_operator) do
            case Nova.Array.head(Nova.Array.drop(2, tokens)) do
              {:just, close_tok} -> if ((close_tok.token_type == :tok_delimiter) and (close_tok.value == ")")) do
                  success(Nova.Compiler.Ast.import_value(Nova.Runtime.append("(", Nova.Runtime.append(op_tok.value, ")"))), Nova.Array.drop(3, tokens))
                else
                  parse_normal_import_item.(tokens)
                end
              _ -> parse_normal_import_item.(tokens)
            end
          else
            parse_normal_import_item.(tokens)
          end
        _ -> parse_normal_import_item.(tokens)
      end
    else
      parse_normal_import_item.(tokens)
    end
  _ -> parse_normal_import_item.(tokens)
end
  end

  def parse_import_spec(tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "..")) do
               case expect_delimiter(Nova.Array.drop(1, tokens), ")") do
       {:left, err} -> {:left, err}
       {:right, {:tuple, _, rest}} ->
success(Nova.Compiler.Ast.import_all, rest)
     end
        else
               case parse_separated((&parse_identifier_name/1), fn tok -> expect_delimiter(tok, ",") end, tokens) do
       {:left, err} -> {:left, err}
       {:right, {:tuple, names, rest}} ->
         case expect_delimiter(rest, ")") do
           {:left, err} -> {:left, err}
           {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.import_some(names), rest_prime)
         end
     end
        end
      _ ->     case parse_separated((&parse_identifier_name/1), fn tok -> expect_delimiter(tok, ",") end, tokens) do
      {:left, err} -> {:left, err}
      {:right, {:tuple, names, rest}} ->
        case expect_delimiter(rest, ")") do
          {:left, err} -> {:left, err}
          {:right, {:tuple, _, rest_prime}} ->
success(Nova.Compiler.Ast.import_some(names), rest_prime)
        end
    end
    end
  end

  def parse_foreign_import_simple(tokens) do
      case expect_keyword(tokens, "foreign") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
      case expect_keyword(rest, "import") do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, rest_prime}} ->
          case parse_identifier_name(rest_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, name, rest_prime_prime}} ->
              case expect_operator(rest_prime_prime, "::") do
                {:left, err} -> {:left, err}
                {:right, {:tuple, _, rest_prime_prime_prime}} ->
                  case parse_type(rest_prime_prime_prime) do
                    {:left, err} -> {:left, err}
                    {:right, {:tuple, ty, rest4}} ->
success(Nova.Compiler.Ast.decl_foreign_import(%{module_name: "", function_name: name, alias_: {:just, name}, type_signature: ty}), drop_newlines(rest4))
                  end
              end
          end
      end
  end
  end

  def parse_data_declaration(tokens) do
      case expect_keyword(tokens, "data") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
      case parse_identifier_name(rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, name, rest_prime}} ->
          case parse_many((&parse_identifier_name/1), rest_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, vars, rest_prime_prime}} ->
                            rest_prime_prime_prime = skip_newlines(rest_prime_prime)
              case expect_operator(rest_prime_prime_prime, "=") do
                {:left, err} -> {:left, err}
                {:right, {:tuple, _, rest4}} ->
                                    rest5 = skip_newlines(rest4)
                  case parse_data_constructors(rest5) do
                    {:left, err} -> {:left, err}
                    {:right, {:tuple, ctors, rest6}} ->
success(Nova.Compiler.Ast.decl_data_type(%{name: name, type_vars: vars, constructors: ctors}), rest6)
                  end
              end
          end
      end
  end
  end

  def parse_data_constructors(tokens) do
    parse_separated((&parse_data_constructor/1), fn t -> expect_operator(t, "|") end, tokens)
  end

  def parse_data_constructor(tokens) do
    
      field_to_data_field = fn f -> %{label: f.label, ty: f.ty} end
      type_to_data_field = fn ty -> %{label: "", ty: ty} end
      case parse_identifier_name(tokens) do
  {:left, err} -> {:left, err}
  {:right, {:tuple, name, rest}} ->
        rest_prime = skip_newlines(rest)
case Nova.Array.head(rest_prime) do
          {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "{")) do
                     case parse_braced_record_fields(rest_prime) do
         {:left, err} -> {:left, err}
         {:right, {:tuple, fields, rest_prime_prime}} ->
success(%{name: name, fields: Nova.Runtime.map(field_to_data_field, fields), is_record: true}, rest_prime_prime)
       end
            else
                     case parse_many((&parse_type_atom/1), rest_prime) do
         {:left, err} -> {:left, err}
         {:right, {:tuple, field_types, rest_prime_prime}} ->
success(%{name: name, fields: Nova.Runtime.map(type_to_data_field, field_types), is_record: false}, rest_prime_prime)
       end
            end
          _ ->       case parse_many((&parse_type_atom/1), rest_prime) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, field_types, rest_prime_prime}} ->
success(%{name: name, fields: Nova.Runtime.map(type_to_data_field, field_types), is_record: false}, rest_prime_prime)
      end
        end
end
  end

  def parse_braced_record_fields(tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if ((t.token_type == :tok_delimiter) and (t.value == "{")) do
               case parse_separated((&parse_record_constructor_field/1), fn tok -> expect_delimiter(tok, ",") end, Nova.Array.drop(1, tokens)) do
       {:left, err} -> {:left, err}
       {:right, {:tuple, fields, rest}} ->
         case expect_delimiter(rest, "}") do
           {:left, err} -> {:left, err}
           {:right, {:tuple, _, rest_prime}} ->
success(fields, rest_prime)
         end
     end
        else
          failure("Expected '{' for record constructor")
        end
      _ -> failure("Expected '{' for record constructor")
    end
  end

  def parse_record_constructor_field(tokens) do
      case parse_identifier_name(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, label, rest}} ->
      case expect_operator(rest, "::") do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, rest_prime}} ->
                    rest_prime_prime = skip_newlines(rest_prime)
          case parse_type(rest_prime_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, ty, rest_prime_prime_prime}} ->
success(%{label: label, ty: ty}, rest_prime_prime_prime)
          end
      end
  end
  end

  def parse_type_alias(tokens) do
      case expect_keyword(tokens, "type") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
      case parse_identifier_name(rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, name, rest_prime}} ->
          case parse_many((&parse_identifier_name/1), rest_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, vars, rest_prime_prime}} ->
              case expect_operator(rest_prime_prime, "=") do
                {:left, err} -> {:left, err}
                {:right, {:tuple, _, rest_prime_prime_prime}} ->
                                    rest4 = skip_newlines(rest_prime_prime_prime)
                  case parse_type(rest4) do
                    {:left, err} -> {:left, err}
                    {:right, {:tuple, aliased, rest5}} ->
success(Nova.Compiler.Ast.decl_type_alias(%{name: name, type_vars: vars, ty: aliased}), rest5)
                  end
              end
          end
      end
  end
  end

  def parse_type_class(tokens) do
      case expect_keyword(tokens, "class") do
    {:left, err} -> {:left, err}
    {:right, {:tuple, _, rest}} ->
            {:tuple, rest_prime, _} = skip_superclass_constraints(rest)
      case parse_identifier_name(rest_prime) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, name, rest_prime_prime}} ->
                    {:tuple, rest_prime_prime_prime, kind} = maybe_parse_class_kind(rest_prime_prime)
          case parse_many((&parse_identifier_name/1), rest_prime_prime_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, vars, rest4}} ->
                            rest5 = skip_newlines(rest4)
case Nova.Array.head(rest5) do
                              {:just, t} -> if ((t.token_type == :tok_keyword) and (t.value == "where")) do
                                                   case parse_many((&parse_type_signature/1), Nova.Array.drop(1, rest5)) do
                   {:left, err} -> {:left, err}
                   {:right, {:tuple, methods, rest6}} ->
success(Nova.Compiler.Ast.decl_type_class(%{name: name, type_vars: vars, methods: methods, kind: kind}), rest6)
                 end
                                else
                                  success(Nova.Compiler.Ast.decl_type_class(%{name: name, type_vars: vars, methods: [], kind: kind}), rest5)
                                end
                              _ -> success(Nova.Compiler.Ast.decl_type_class(%{name: name, type_vars: vars, methods: [], kind: kind}), rest5)
                            end
          end
      end
  end
  end

  def skip_superclass_constraints(tokens) do
    
      tokens_prime = skip_newlines(tokens)
      %{init: before, rest: after_} = Nova.Array.span(fn t -> not(((t.token_type == :tok_operator) and (t.value == "<="))) end, tokens_prime)
      case Nova.Array.head(after_) do
  {:just, t} -> if ((t.token_type == :tok_operator) and (t.value == "<=")) do
      {:tuple, Nova.Array.drop(1, after_), before}
    else
      {:tuple, tokens_prime, []}
    end
  _ -> {:tuple, tokens_prime, []}
end
  end

  def maybe_parse_class_kind(tokens) do
    case expect_operator(tokens, "::") do
      {:right, ({:tuple, _, rest})} -> case parse_type(rest) do
          {:right, ({:tuple, _, rest_prime})} -> {:tuple, rest_prime, :nothing}
          {:left, _} -> {:tuple, tokens, :nothing}
        end
      {:left, _} -> {:tuple, tokens, :nothing}
    end
  end

  def parse_type_class_instance(tokens) do
    
      parse_unnamed_instance = fn rest -> fn is_derived ->       rest_prime = drop_instance_constraints(rest)
   case parse_identifier_name(rest_prime) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, class_name, rest_prime_prime}} ->
       case parse_type(rest_prime_prime) do
         {:left, err} -> {:left, err}
         {:right, {:tuple, ty, rest_prime_prime_prime}} ->
case expect_keyword(rest_prime_prime_prime, "where") do
                        {:right, ({:tuple, _, rest4})} ->              case parse_many((&parse_function_declaration_raw/1), rest4) do
               {:left, err} -> {:left, err}
               {:right, {:tuple, methods, rest5}} ->
success(Nova.Compiler.Ast.decl_type_class_instance(%{class_name: class_name, ty: ty, methods: methods, derived: is_derived}), rest5)
             end
                        {:left, _} -> if is_derived do
                            success(Nova.Compiler.Ast.decl_type_class_instance(%{class_name: class_name, ty: ty, methods: [], derived: is_derived}), rest_prime_prime_prime)
                          else
                            failure("Expected 'where' clause for non-derived instance")
                          end
                      end
       end
   end end end
      extract_class_name = Nova.Runtime.fix(fn extract_class_name -> fn __arg0__ -> case __arg0__ do
        ({:ty_expr_con, name}) -> name
        ({:ty_expr_app, fn_, _}) -> extract_class_name.(fn_)
        _ -> "Unknown"
      end end end)
      {:tuple, tokens_prime, derived} = case Nova.Array.head(tokens) do
  {:just, t} -> if ((t.token_type == :tok_keyword) and (t.value == "derive")) do
      {:tuple, Nova.Array.drop(1, tokens), true}
    else
      {:tuple, tokens, false}
    end
  _ -> {:tuple, tokens, false}
end
case expect_keyword(tokens_prime, "instance") do
  {:left, err} -> {:left, err}
  {:right, {:tuple, _, rest}} ->
        rest_prime = drop_newlines(rest)
case Nova.Array.head(rest_prime) do
          {:just, t} -> if (t.token_type == :tok_identifier) do
              case Nova.Array.head(Nova.Array.drop(1, rest_prime)) do
                {:just, t_prime} -> if ((t_prime.token_type == :tok_operator) and (t_prime.value == "::")) do
                                        rest_prime_prime = drop_instance_constraints(Nova.Array.drop(2, rest_prime))
          case parse_type(rest_prime_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, ty, rest_prime_prime_prime}} ->
case expect_keyword(rest_prime_prime_prime, "where") do
                              {:right, ({:tuple, _, rest4})} ->                 case parse_many((&parse_function_declaration_raw/1), rest4) do
                  {:left, err} -> {:left, err}
                  {:right, {:tuple, methods, rest5}} ->
                                        class_name = extract_class_name.(ty)
success(Nova.Compiler.Ast.decl_type_class_instance(%{class_name: class_name, ty: ty, methods: methods, derived: derived}), rest5)
                end
                              {:left, _} -> if derived do
                                                                    class_name = extract_class_name.(ty)
success(Nova.Compiler.Ast.decl_type_class_instance(%{class_name: class_name, ty: ty, methods: [], derived: derived}), rest_prime_prime_prime)
                                else
                                  failure("Expected 'where' clause for non-derived instance")
                                end
                            end
          end
                  else
                    parse_unnamed_instance.(rest_prime).(derived)
                  end
                _ -> parse_unnamed_instance.(rest_prime).(derived)
              end
            else
              parse_unnamed_instance.(rest_prime).(derived)
            end
          _ -> parse_unnamed_instance.(rest_prime).(derived)
        end
end
  end

  def drop_instance_constraints(tokens) do
    
      tokens_prime = skip_newlines(tokens)
      %{init: before, rest: after_} = Nova.Array.span(fn t -> not(((t.token_type == :tok_operator) and (t.value == "=>"))) end, tokens_prime)
      case Nova.Array.head(after_) do
  {:just, t} -> if ((t.token_type == :tok_operator) and ((t.value == "=>") and (Nova.Array.length(before) < 20))) do
      skip_newlines(Nova.Array.drop(1, after_))
    else
      tokens_prime
    end
  _ -> tokens_prime
end
  end

  def parse_function_with_type_signature(tokens) do
        tokens_prime = drop_newlines(tokens)
case Nova.Array.head(tokens_prime) do
      {:just, t} -> if (t.token_type == :tok_identifier) do
                    name = t.value
case expect_operator(Nova.Array.drop(1, tokens_prime), "::") do
            {:right, ({:tuple, _, rest})} ->               {:tuple, type_tokens, rest_prime} = split_type_and_rest(rest, name)
       case parse_type(strip_newlines(type_tokens)) do
         {:left, err} -> {:left, err}
         {:right, {:tuple, ty, _}} ->
           case parse_function_declaration_raw(rest_prime) do
             {:left, err} -> {:left, err}
             {:right, {:tuple, fun, rest_prime_prime}} ->
if (fun.name == name) do
                                success(Nova.Compiler.Ast.decl_function(%{name: fun.name, parameters: fun.parameters, body: fun.body, guards: fun.guards, type_signature: {:just, %{name: name, type_vars: [], constraints: [], ty: ty}}}), rest_prime_prime)
                              else
                                failure("Function name mismatch")
                              end
           end
       end
            {:left, _} -> failure("Expected '::'")
          end
        else
          failure("Expected identifier")
        end
      _ -> failure("Expected identifier")
    end
  end

  def split_type_and_rest(tokens, name) do
    
      go = Nova.Runtime.fix2(fn go -> fn toks -> fn acc -> case Nova.Array.head(toks) do
        :nothing -> {:tuple, acc, toks}
        {:just, t} -> if ((t.token_type == :tok_identifier) and ((t.value == name) and (t.column == 1))) do
            {:tuple, acc, toks}
          else
            if (t.token_type == :tok_newline) do
              go.(Nova.Array.drop(1, toks)).(acc)
            else
              go.(Nova.Array.drop(1, toks)).(Nova.Array.snoc(acc, t))
            end
          end
      end  end end end)
      go.(tokens).([])
  end

  def parse_function_declaration(tokens) do
      case parse_function_declaration_raw(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, fun, rest}} ->
success(Nova.Compiler.Ast.decl_function(fun), rest)
  end
  end

  def parse_function_declaration_raw(tokens) do
      case parse_identifier_name(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, name, rest}} ->
      case parse_many((&parse_simple_pattern/1), rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, params, rest_prime}} ->
                    rest_prime_prime = skip_newlines(rest_prime)
case Nova.Array.head(rest_prime_prime) do
                      {:just, tok} -> if ((tok.token_type == :tok_operator) and (tok.value == "|")) do
                          parse_guarded_function(name, params, rest_prime_prime)
                        else
                                       case expect_operator(rest_prime, "=") do
               {:left, err} -> {:left, err}
               {:right, {:tuple, _, rest_prime_prime_prime}} ->
case Nova.Array.head(rest_prime_prime_prime) do
                                    {:just, first_tok} ->                    case parse_expression(rest_prime_prime_prime) do
                     {:left, err} -> {:left, err}
                     {:right, {:tuple, body, rest4}} ->
                       case maybe_parse_where(rest4, first_tok.column, body) do
                         {:left, err} -> {:left, err}
                         {:right, {:tuple, body_prime, rest5}} ->
success(%{name: name, parameters: params, body: body_prime, guards: [], type_signature: :nothing}, rest5)
                       end
                   end
                                    :nothing -> failure("Expected expression")
                                  end
             end
                        end
                      _ ->             case expect_operator(rest_prime, "=") do
              {:left, err} -> {:left, err}
              {:right, {:tuple, _, rest_prime_prime_prime}} ->
case Nova.Array.head(rest_prime_prime_prime) do
                                  {:just, first_tok} ->                   case parse_expression(rest_prime_prime_prime) do
                    {:left, err} -> {:left, err}
                    {:right, {:tuple, body, rest4}} ->
                      case maybe_parse_where(rest4, first_tok.column, body) do
                        {:left, err} -> {:left, err}
                        {:right, {:tuple, body_prime, rest5}} ->
success(%{name: name, parameters: params, body: body_prime, guards: [], type_signature: :nothing}, rest5)
                      end
                  end
                                  :nothing -> failure("Expected expression")
                                end
            end
                    end
      end
  end
  end

  def parse_guarded_function(name, params, tokens) do
      case parse_guarded_exprs(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, guards, rest}} ->
            body = Nova.Compiler.Ast.expr_var("__guarded__")
success(%{name: name, parameters: params, body: body, guards: guards, type_signature: :nothing}, rest)
  end
  end

  def parse_guarded_exprs(tokens) do
    
      parse_guarded_exprs_acc = Nova.Runtime.fix2(fn parse_guarded_exprs_acc -> fn toks -> fn acc -> 
        toks_prime = skip_newlines(toks)
        case Nova.Array.head(toks_prime) do
  {:just, tok} -> if ((tok.token_type == :tok_operator) and (tok.value == "|")) do
         case parse_one_guarded_expr(Nova.Array.drop(1, toks_prime)) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, guard, rest}} ->
parse_guarded_exprs_acc.(rest).(Nova.Array.snoc(acc, guard))
   end
    else
      success(acc, toks_prime)
    end
  _ -> success(acc, toks_prime)
end  end end end)
      parse_guarded_exprs_acc.(tokens).([])
  end

  def parse_one_guarded_expr(tokens) do
      case parse_guard_clauses(tokens, []) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, clauses, rest}} ->
      case expect_operator(rest, "=") do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, rest_prime}} ->
          case parse_expression(rest_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, body, rest_prime_prime}} ->
success(%{guards: clauses, body: body}, rest_prime_prime)
          end
      end
  end
  end

  def parse_guard_clauses(tokens, acc) do
      case parse_one_guard_clause(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, clause, rest}} ->
            rest_prime = skip_newlines(rest)
case Nova.Array.head(rest_prime) do
              {:just, tok} -> if ((tok.token_type == :tok_delimiter) and (tok.value == ",")) do
                  parse_guard_clauses(Nova.Array.drop(1, rest_prime), Nova.Array.snoc(acc, clause))
                else
                  success(Nova.Array.snoc(acc, clause), rest_prime)
                end
              _ -> success(Nova.Array.snoc(acc, clause), rest_prime)
            end
  end
  end

  def parse_one_guard_clause(tokens) do
    case try_pattern_guard(tokens) do
      {:right, result} -> {:right, result}
      {:left, _} -> case parse_func_guard_expr(tokens) do
          {:right, ({:tuple, expr, rest})} -> success(Nova.Compiler.Ast.guard_expr(expr), rest)
          {:left, err} -> {:left, err}
        end
    end
  end

  def try_pattern_guard(tokens) do
      case parse_pattern(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, pat, rest}} ->
            rest_prime = skip_newlines(rest)
case Nova.Array.head(rest_prime) do
              {:just, tok} -> if ((tok.token_type == :tok_operator) and (tok.value == "<-")) do
                           case parse_func_guard_expr(Nova.Array.drop(1, rest_prime)) do
           {:left, err} -> {:left, err}
           {:right, {:tuple, expr, rest_prime_prime}} ->
success(Nova.Compiler.Ast.guard_pat(pat, expr), rest_prime_prime)
         end
                else
                  failure("Expected <- for pattern guard")
                end
              _ -> failure("Expected <- for pattern guard")
            end
  end
  end

  def parse_func_guard_expr(tokens) do
    parse_guard_expr_or(tokens)
  end

  def parse_guard_expr_or(tokens) do
      case parse_guard_expr_and(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
            rest_prime = skip_newlines(rest)
case Nova.Array.head(rest_prime) do
              {:just, tok} -> if ((tok.token_type == :tok_operator) and (tok.value == "||")) do
                           case parse_guard_expr_or(Nova.Array.drop(1, rest_prime)) do
           {:left, err} -> {:left, err}
           {:right, {:tuple, right, rest_prime_prime}} ->
success(Nova.Compiler.Ast.expr_bin_op("||", left, right), rest_prime_prime)
         end
                else
                  success(left, rest_prime)
                end
              _ -> success(left, rest_prime)
            end
  end
  end

  def parse_guard_expr_and(tokens) do
      case parse_guard_expr_comparison(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
            rest_prime = skip_newlines(rest)
case Nova.Array.head(rest_prime) do
              {:just, tok} -> if ((tok.token_type == :tok_operator) and (tok.value == "&&")) do
                           case parse_guard_expr_and(Nova.Array.drop(1, rest_prime)) do
           {:left, err} -> {:left, err}
           {:right, {:tuple, right, rest_prime_prime}} ->
success(Nova.Compiler.Ast.expr_bin_op("&&", left, right), rest_prime_prime)
         end
                else
                  success(left, rest_prime)
                end
              _ -> success(left, rest_prime)
            end
  end
  end

  def parse_guard_expr_comparison(tokens) do
    
      is_comparison_op = fn op -> Nova.Array.elem(op, ["==", "/=", "<", ">", "<=", ">="]) end
      case parse_guard_expr_app(tokens) do
  {:left, err} -> {:left, err}
  {:right, {:tuple, left, rest}} ->
        rest_prime = skip_newlines(rest)
case Nova.Array.head(rest_prime) do
          {:just, tok} -> if ((tok.token_type == :tok_operator) and is_comparison_op.(tok.value)) do
                     case parse_guard_expr_app(Nova.Array.drop(1, rest_prime)) do
         {:left, err} -> {:left, err}
         {:right, {:tuple, right, rest_prime_prime}} ->
success(Nova.Compiler.Ast.expr_bin_op(tok.value, left, right), rest_prime_prime)
       end
            else
              success(left, rest_prime)
            end
          _ -> success(left, rest_prime)
        end
end
  end

  def parse_guard_expr_app(tokens) do
      case parse_guard_expr_atom(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, first, rest}} ->
parse_guard_expr_app_rest(first, rest)
  end
  end

  def parse_guard_expr_app_rest(func, tokens) do
    
      tokens_prime = skip_newlines(tokens)
      case Nova.Array.head(tokens_prime) do
  {:just, tok} -> if ((tok.token_type == :tok_operator) and Nova.Array.elem(tok.value, ["=", ",", "|", "||", "&&", "==", "/=", "<", ">", "<=", ">="])) do
      success(func, tokens_prime)
    else
      if (tok.token_type == :tok_newline) do
        success(func, tokens_prime)
      else
        case parse_guard_expr_atom(tokens_prime) do
          {:right, ({:tuple, arg, rest})} -> parse_guard_expr_app_rest(Nova.Compiler.Ast.expr_app(func, arg), rest)
          {:left, _} -> success(func, tokens_prime)
        end
      end
    end
  _ -> case parse_guard_expr_atom(tokens_prime) do
      {:right, ({:tuple, arg, rest})} -> parse_guard_expr_app_rest(Nova.Compiler.Ast.expr_app(func, arg), rest)
      {:left, _} -> success(func, tokens_prime)
    end
end
  end

  def parse_guard_expr_atom(tokens) do
    
      tokens_prime = skip_newlines(tokens)
      case Nova.Array.head(tokens_prime) do
  {:just, tok} -> if (tok.token_type == :tok_identifier) do
      
        rest = Nova.Array.drop(1, tokens_prime)
        base_expr = Nova.Compiler.Ast.expr_var(tok.value)
        parse_record_access_chain(base_expr, rest)
    else
      if (tok.token_type == :tok_number) do
        case Nova.String.to_int(tok.value) do
          {:just, n} -> success(Nova.Compiler.Ast.expr_lit(Nova.Compiler.Ast.lit_int(n)), Nova.Array.drop(1, tokens_prime))
          :nothing -> failure(Nova.Runtime.append("Invalid integer: ", tok.value))
        end
      else
        if (tok.token_type == :tok_string) do
          success(Nova.Compiler.Ast.expr_lit(Nova.Compiler.Ast.lit_string(tok.value)), Nova.Array.drop(1, tokens_prime))
        else
          if ((tok.token_type == :tok_keyword) and (tok.value == "true")) do
            success(Nova.Compiler.Ast.expr_lit(Nova.Compiler.Ast.lit_bool(true)), Nova.Array.drop(1, tokens_prime))
          else
            if ((tok.token_type == :tok_keyword) and (tok.value == "false")) do
              success(Nova.Compiler.Ast.expr_lit(Nova.Compiler.Ast.lit_bool(false)), Nova.Array.drop(1, tokens_prime))
            else
              if ((tok.token_type == :tok_keyword) and (tok.value == "otherwise")) do
                success(Nova.Compiler.Ast.expr_lit(Nova.Compiler.Ast.lit_bool(true)), Nova.Array.drop(1, tokens_prime))
              else
                if ((tok.token_type == :tok_delimiter) and (tok.value == "(")) do
                           case parse_expression(Nova.Array.drop(1, tokens_prime)) do
           {:left, err} -> {:left, err}
           {:right, {:tuple, expr, rest}} ->
             case expect_delimiter(rest, ")") do
               {:left, err} -> {:left, err}
               {:right, {:tuple, _, rest_prime}} ->
success(expr, rest_prime)
             end
         end
                else
                  if ((tok.token_type == :tok_operator) and (tok.value == ".")) do
                    case Nova.Array.head(Nova.Array.drop(1, tokens_prime)) do
                      {:just, fld} -> if (fld.token_type == :tok_identifier) do
                          success(Nova.Compiler.Ast.expr_section(Nova.Runtime.append(".", fld.value)), Nova.Array.drop(2, tokens_prime))
                        else
                          failure("Expected field name after .")
                        end
                      _ -> failure("Expected field name after .")
                    end
                  else
                    failure("Expected guard expression atom")
                  end
                end
              end
            end
          end
        end
      end
    end
  _ -> failure("Expected guard expression atom")
end
  end

  def parse_record_access_chain(expr, tokens) do
    case Nova.Array.head(tokens) do
      {:just, tok} -> if ((tok.token_type == :tok_operator) and (tok.value == ".")) do
          case Nova.Array.head(Nova.Array.drop(1, tokens)) do
            {:just, fld} -> if (fld.token_type == :tok_identifier) do
                parse_record_access_chain(Nova.Compiler.Ast.expr_record_access(expr, fld.value), Nova.Array.drop(2, tokens))
              else
                success(expr, tokens)
              end
            _ -> success(expr, tokens)
          end
        else
          success(expr, tokens)
        end
      _ -> success(expr, tokens)
    end
  end

  def maybe_parse_where(tokens, _, body) do
        tokens_prime = skip_newlines(tokens)
case Nova.Array.head(tokens_prime) do
      {:just, t} -> if ((t.token_type == :tok_keyword) and (t.value == "where")) do
          
            where_col = t.column
            rest = skip_newlines(Nova.Array.drop(1, tokens_prime))
            case Nova.Array.head(rest) do
  {:just, first_tok} -> if (first_tok.column > where_col) do
         case collect_where_bindings(rest, where_col, []) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, bindings, rest_prime}} ->
success(Nova.Compiler.Ast.expr_let(bindings, body), rest_prime)
   end
    else
      success(body, tokens_prime)
    end
  _ -> success(body, tokens_prime)
end
        else
          success(body, tokens)
        end
      _ -> success(body, tokens)
    end
  end

  def collect_where_bindings(tokens, where_col, acc) do
    
      wrap_lambda = fn params -> fn body -> case Nova.Array.length(params) do
        0 -> body
        _ -> Nova.Compiler.Ast.expr_lambda(params, body)
      end end end
      is_type_signature_line = fn toks -> case Nova.Array.head(toks) do
        {:just, t1} -> if (t1.token_type == :tok_identifier) do
            case Nova.Array.head(Nova.Array.drop(1, toks)) do
              {:just, t2} -> if ((t2.token_type == :tok_operator) and (t2.value == "::")) do
                  true
                else
                  false
                end
              _ -> false
            end
          else
            false
          end
        _ -> false
      end end
      skip_to_next_line = Nova.Runtime.fix(fn skip_to_next_line -> fn toks -> case Nova.Array.head(toks) do
        :nothing -> toks
        {:just, t} -> if (t.token_type == :tok_newline) do
            
              rest = Nova.Array.drop(1, toks)
              case Nova.Array.head(rest) do
  {:just, t_prime} -> if (t_prime.column <= where_col) do
      toks
    else
      if (t_prime.token_type == :tok_newline) do
        skip_to_next_line.(rest)
      else
        rest
      end
    end
  _ -> rest
end
          else
            skip_to_next_line.(Nova.Array.drop(1, toks))
          end
      end  end end)
      tokens_prime = skip_newlines(tokens)
case Nova.Array.head(tokens_prime) do
  {:just, t} -> if (t.column > where_col) do
      case is_type_signature_line.(tokens_prime) do
        true ->           rest = skip_to_next_line.(tokens_prime)
collect_where_bindings(rest, where_col, acc)
        false ->      case parse_function_declaration_raw(tokens_prime) do
       {:left, err} -> {:left, err}
       {:right, {:tuple, fun, rest}} ->
                  binding = %{pattern: Nova.Compiler.Ast.pat_var(fun.name), value: wrap_lambda.(fun.parameters).(fun.body), type_ann: :nothing}
collect_where_bindings(rest, where_col, Nova.Array.snoc(acc, binding))
     end
      end
    else
      success(acc, tokens_prime)
    end
  _ -> success(acc, tokens_prime)
end
  end

  def parse_type_signature_decl(tokens) do
      case parse_type_signature(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, sig, rest}} ->
success(Nova.Compiler.Ast.decl_type_sig(sig), rest)
  end
  end

  def parse_declarations(tokens) do
    parse_declarations_acc(tokens, [])
  end

  def parse_declarations_acc(tokens, acc) do
        tokens_prime = skip_newlines(tokens)
case Nova.Array.head(tokens_prime) do
      :nothing -> success(acc, [])
      _ -> case parse_declaration(tokens_prime) do
          {:right, ({:tuple, decl, rest})} -> parse_declarations_acc(rest, Nova.Array.snoc(acc, decl))
          {:left, _} -> if (Nova.Array.length(acc) > 0) do
              success(acc, tokens_prime)
            else
              failure("Failed to parse declaration")
            end
        end
    end
  end

  def parse_module(tokens) do
      case parse_module_header(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, header, rest}} ->
case header do
              {:decl_module, m} ->         case parse_declarations(rest) do
          {:left, err} -> {:left, err}
          {:right, {:tuple, decls, rest_prime}} ->
                        rest_prime_prime = skip_newlines(rest_prime)
case Nova.Array.length(rest_prime_prime) do
                          0 -> success(%{name: m.name, declarations: decls}, [])
                          _ -> failure("Unexpected tokens after module")
                        end
        end
              _ -> failure("Expected module declaration")
            end
  end
  end
end
