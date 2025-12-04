defmodule Nova.Compiler.Parser do
  # import Prelude

  # import Data.Array

  # import Data.List

  # import Data.List

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

  # @type parse_result :: either()(string())((tuple()(a)((array()(token())))))

  def success(a, tokens) do
    {:right, ({:tuple, a, tokens})}
  end

  def failure(msg) do
    {:left, msg}
  end



  def skip_newlines(tokens) do
    Nova.Array.drop_while((fn t -> (t.token_type == :tok_newline) end), tokens)
  end



  def drop_newlines(auto_arg0) do
    skip_newlines(auto_arg0)
  end



  def strip_newlines(auto_arg0) do
    Nova.Array.filter((fn t -> (t.token_type != :tok_newline) end), auto_arg0)
  end



  def is_lower_case(s) do
    case Nova.String.char_at(0, s) do
      {:just, c} -> (((c >= ?a) and c) <= ?z)
      :nothing -> false
    end
  end



  def expect_keyword(tokens, expected) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_keyword) and t.value) == expected) do
      success(expected, (Nova.Array.drop(1, ts)))
    else
      Nova.Runtime.append(Nova.Runtime.append(((&failure/1) |> "Expected keyword '"), expected), "'")
    end
  :nothing -> Nova.Runtime.append(Nova.Runtime.append(((&failure/1) |> "Expected keyword '"), expected), "'")
end
  end



  def expect_identifier(tokens, expected) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_identifier) and t.value) == expected) do
      success(expected, (Nova.Array.drop(1, ts)))
    else
      Nova.Runtime.append(Nova.Runtime.append(((&failure/1) |> "Expected identifier '"), expected), "'")
    end
  :nothing -> Nova.Runtime.append(Nova.Runtime.append(((&failure/1) |> "Expected identifier '"), expected), "'")
end
  end



  def expect_operator(tokens, expected) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_operator) and t.value) == expected) do
      success(expected, (Nova.Array.drop(1, ts)))
    else
      Nova.Runtime.append(Nova.Runtime.append(((&failure/1) |> "Expected operator '"), expected), "'")
    end
  :nothing -> Nova.Runtime.append(Nova.Runtime.append(((&failure/1) |> "Expected operator '"), expected), "'")
end
  end



  def expect_delimiter(tokens, expected) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == expected) do
      success(expected, (Nova.Array.drop(1, ts)))
    else
      Nova.Runtime.append(Nova.Runtime.append(((&failure/1) |> "Expected delimiter '"), expected), "'")
    end
  :nothing -> Nova.Runtime.append(Nova.Runtime.append(((&failure/1) |> "Expected delimiter '"), expected), "'")
end
  end



  def expect_colon(tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if (((t.token_type == :tok_operator) and t.value) == ":") do
          success(":", (Nova.Array.drop(1, tokens)))
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
      success((Nova.Compiler.Ast.expr_var(t.value)), (Nova.Array.drop(1, ts)))
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
      success(t.value, (Nova.Array.drop(1, ts)))
    else
      failure("Expected identifier")
    end
  :nothing -> failure("Expected identifier")
end
  end



  def parse_label(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_identifier) or t.token_type) == :tok_keyword) do
      success(t.value, (Nova.Array.drop(1, ts)))
    else
      failure("Expected label")
    end
  :nothing -> failure("Expected label")
end
  end



  def parse_literal(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> case t.token_type do
      :tok_number -> if Nova.String.contains((Nova.String.pattern(".")), t.value) do
          success((Nova.Compiler.Ast.lit_number((read_number.(t.value)))), (Nova.Array.drop(1, ts)))
        else
          success((Nova.Compiler.Ast.lit_int((read_int.(t.value)))), (Nova.Array.drop(1, ts)))
        end
      :tok_string -> success((Nova.Compiler.Ast.lit_string(t.value)), (Nova.Array.drop(1, ts)))
      :tok_char -> success((Nova.Compiler.Ast.lit_char((first_char.(t.value)))), (Nova.Array.drop(1, ts)))
      :tok_identifier -> if (t.value == "true") do
          success((Nova.Compiler.Ast.lit_bool(true)), (Nova.Array.drop(1, ts)))
        else
          if (t.value == "false") do
            success((Nova.Compiler.Ast.lit_bool(false)), (Nova.Array.drop(1, ts)))
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
      success(t.value, (Nova.Array.drop(1, ts)))
    else
      failure("Expected string literal")
    end
  :nothing -> failure("Expected string literal")
end
  end

  def parse_any(parsers, tokens) do
    go.(parsers)
  end

  def parse_many(parser, tokens) do
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
          {:right, ({:tuple, item, rest_prime})} -> parse_separated_rest(parser, separator, rest_prime, (Nova.Array.snoc(acc, item)))
          {:left, _} -> failure("Expected item after separator")
        end
      {:left, _} -> success(acc, tokens)
    end
  end



  def parse_qualified_identifier(tokens) do
    
      tokens_prime = skip_newlines(tokens)
      case Nova.Array.head(tokens_prime) do
  {:just, t} -> if (((t.token_type == :tok_identifier) and t.column) == 1) do
      case Nova.Array.head((Nova.Array.drop(1, tokens_prime))) do
        {:just, t_prime} -> if (((t_prime.token_type == :tok_operator) and t_prime.value) == "::") do
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



  def parse_qualified_identifier_for_guard(tokens) do
    
      tokens_prime = skip_newlines(tokens)
      case parse_separated((&parse_identifier_name/1), (fn t -> expect_operator(t, ".") end), tokens_prime) do
  {:left, err} -> {:left, err}
  {:right, ({:tuple, parts, rest})} -> case Nova.Array.length(parts) do
      0 -> failure("Expected identifier")
      1 -> case Nova.Array.head(parts) do
          {:just, name} -> success((Nova.Compiler.Ast.expr_var(name)), rest)
          :nothing -> failure("Expected identifier")
        end
      _ -> case Nova.Array.head(parts) do
          {:just, first} -> if not((is_upper_case.(first))) do
              
                base_expr = Nova.Compiler.Ast.expr_var(first)
                fields = Nova.Array.drop(1, parts)
                success((Nova.Array.foldl(fn a, b -> Nova.Compiler.Ast.expr_record_access(a, b) end, base_expr, fields)), rest)
            else
              
                all_but_last = Nova.Array.take(((Nova.Array.length(parts) - 1)), parts)
                last_name = Nova.Array.last(parts)
                case last_name do
  {:just, name} -> success((Nova.Compiler.Ast.expr_qualified((Nova.String.join_with(".", all_but_last)), name)), rest)
  :nothing -> failure("Expected qualified identifier")
end
            end
          _ -> 
              all_but_last = Nova.Array.take(((Nova.Array.length(parts) - 1)), parts)
              last_name = Nova.Array.last(parts)
              case last_name do
  {:just, name} -> success((Nova.Compiler.Ast.expr_qualified((Nova.String.join_with(".", all_but_last)), name)), rest)
  :nothing -> failure("Expected qualified identifier")
end
        end
    end
end
  end



  def parse_type(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_keyword) and t.value) == "forall") do
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
                  success((Nova.Compiler.Ast.ty_expr_for_all((List.from_foldable(vars)), ty)), rest_prime_prime_prime)
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
  {:just, t} -> if (((t.token_type == :tok_operator) and t.value) == "->") do
         case parse_function_type((Nova.Array.drop(1, ts))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, right, rest_prime}} ->
       success((Nova.Compiler.Ast.ty_expr_arrow(left, right)), rest_prime)
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
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "{") do
         case parse_separated((&parse_record_field/1), (fn tok -> expect_delimiter(tok, ",") end), (Nova.Array.drop(1, ts))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, fields, rest}} ->
       case expect_delimiter(rest, "}") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
           success((Nova.Compiler.Ast.ty_expr_record((List.from_foldable(fields)), :nothing)), rest_prime)
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
              success(({:tuple, label, ty}), rest_prime_prime_prime)
          end
      end
  end
  end



  def parse_list_type(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "[") do
         case parse_type((Nova.Array.drop(1, ts))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, elem_type, rest}} ->
       case expect_delimiter(rest, "]") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
           success((Nova.Compiler.Ast.ty_expr_app((Nova.Compiler.Ast.ty_expr_con("Array")), elem_type)), rest_prime)
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
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "(") do
         case parse_separated((&parse_type/1), (fn tok -> expect_delimiter(tok, ",") end), (Nova.Array.drop(1, ts))) do
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
  _ -> success((Nova.Compiler.Ast.ty_expr_tuple((List.from_foldable(elements)))), rest_prime)
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
  _ -> success((fold_type_app.(base).(args)), rest_prime)
end
       end
   end
    else
      if (((t.token_type == :tok_delimiter) and t.value) == "(") do
            case parse_type((Nova.Array.drop(1, ts))) do
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
    case parse_separated((&parse_identifier_name/1), (fn t -> expect_operator(t, ".") end), tokens) do
      {:left, err} -> {:left, err}
      {:right, ({:tuple, parts, rest})} -> success((Nova.String.join_with(".", parts)), rest)
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
          if (((t.token_type == :tok_delimiter) and t.value) == "{") do
            parse_record_type(tokens)
          else
            if (t.token_type == :tok_identifier) do
                     case parse_qualified_type_name(tokens) do
         {:left, err} -> {:left, err}
         {:right, {:tuple, name, rest}} ->
           success((if is_lower_case(name) do
  Nova.Compiler.Ast.ty_expr_var(name)
else
  Nova.Compiler.Ast.ty_expr_con(name)
end), rest)
       end
            else
              if (((t.token_type == :tok_delimiter) and t.value) == "(") do
                        case parse_type((Nova.Array.drop(1, tokens))) do
          {:left, err} -> {:left, err}
          {:right, {:tuple, ty, rest}} ->
            case expect_delimiter(rest, ")") do
              {:left, err} -> {:left, err}
              {:right, {:tuple, _, rest_prime}} ->
                success(ty, rest_prime)
            end
        end
              else
                if (((t.token_type == :tok_delimiter) and t.value) == "[") do
                           case parse_type((Nova.Array.drop(1, tokens))) do
           {:left, err} -> {:left, err}
           {:right, {:tuple, elem_type, rest}} ->
             case expect_delimiter(rest, "]") do
               {:left, err} -> {:left, err}
               {:right, {:tuple, _, rest_prime}} ->
                 success((Nova.Compiler.Ast.ty_expr_app((Nova.Compiler.Ast.ty_expr_con("Array")), elem_type)), rest_prime)
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
    parse_any([(&parse_record_pattern/1), (&parse_wildcard_pattern/1), (&parse_cons_pattern/1), (&parse_constructor_pattern/1), (&parse_paren_pattern/1), (&parse_tuple_pattern/1), (&parse_list_pattern/1), (&parse_literal_pattern/1), (&parse_as_pattern/1), (&parse_var_pattern/1)], tokens)
  end



  def parse_var_pattern(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (t.token_type == :tok_identifier) do
      success((Nova.Compiler.Ast.pat_var(t.value)), (Nova.Array.drop(1, ts)))
    else
      failure("Expected variable pattern")
    end
  :nothing -> failure("Expected variable pattern")
end
  end



  def parse_as_pattern(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_identifier) and t.value) != "_") do
      case Nova.Array.head((Nova.Array.drop(1, ts))) do
        {:just, t2} -> if (((t2.token_type == :tok_operator) and t2.value) == "@") do
                  case parse_simple_pattern((Nova.Array.drop(2, ts))) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, pat, rest}} ->
          success((Nova.Compiler.Ast.pat_as(t.value, pat)), rest)
      end
          else
            failure("Expected @ for as-pattern")
          end
        _ -> failure("Expected @ for as-pattern")
      end
    else
      failure("Expected identifier for as-pattern")
    end
  :nothing -> failure("Expected identifier for as-pattern")
end
  end



  def parse_wildcard_pattern(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_identifier) and t.value) == "_") do
      success(Nova.Compiler.Ast.pat_wildcard(), (Nova.Array.drop(1, ts)))
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
      success((Nova.Compiler.Ast.pat_lit(lit)), rest)
  end
  end



  def parse_constructor_pattern(tokens) do
      case parse_qualified_constructor_name(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, name, rest}} ->
      if is_capital.(name) do
   case parse_many((&parse_simple_pattern/1), rest) do
   {:left, err} -> {:left, err}
   {:right, {:tuple, args, rest_prime}} ->
     case Nova.Array.length(args) do
  0 -> success((Nova.Compiler.Ast.pat_con(name, [])), rest_prime)
  _ -> success((Nova.Compiler.Ast.pat_con(name, (List.from_foldable(args)))), rest_prime)
end
 end
else
  failure("Expected constructor pattern")
end
  end
  end



  def parse_qualified_constructor_name(tokens) do
    case parse_separated((&parse_identifier_name/1), (fn t -> expect_operator(t, ".") end), tokens) do
      {:left, err} -> {:left, err}
      {:right, ({:tuple, parts, rest})} -> success((Nova.String.join_with(".", parts)), rest)
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
              success((Nova.Compiler.Ast.pat_cons(hd, tl)), rest_prime_prime)
          end
      end
  end
  end



  def parse_tuple_pattern(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "(") do
         case parse_separated((&parse_pattern/1), (fn tok -> expect_delimiter(tok, ",") end), (Nova.Array.drop(1, ts))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, elements, rest}} ->
       case expect_delimiter(rest, ")") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
           success((Nova.Compiler.Ast.pat_record((List.from_foldable((Nova.Array.map_with_index((fn i -> fn p -> {:tuple, (Nova.Runtime.show(i)), p} end end), elements)))))), rest_prime)
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
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "[") do
      case Nova.Array.head((Nova.Array.drop(1, ts))) do
        {:just, t_prime} -> if (((t_prime.token_type == :tok_delimiter) and t_prime.value) == "]") do
            success((Nova.Compiler.Ast.pat_list([])), (Nova.Array.drop(2, ts)))
          else
                  case parse_separated((&parse_pattern/1), (fn tok -> expect_delimiter(tok, ",") end), (Nova.Array.drop(1, ts))) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, elements, rest}} ->
          case expect_delimiter(rest, "]") do
            {:left, err} -> {:left, err}
            {:right, {:tuple, _, rest_prime}} ->
              success((Nova.Compiler.Ast.pat_list((List.from_foldable(elements)))), rest_prime)
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
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "{") do
         case parse_separated((&parse_record_field_pattern/1), (fn tok -> expect_delimiter(tok, ",") end), (Nova.Array.drop(1, ts))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, fields, rest}} ->
       case expect_delimiter(rest, "}") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
           success((Nova.Compiler.Ast.pat_record((List.from_foldable(fields)))), rest_prime)
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
  {:right, ({:tuple, _, rest_prime})} ->   case parse_pattern(rest_prime) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, pat, rest_prime_prime}} ->
      success(({:tuple, label, pat}), rest_prime_prime)
  end
  {:left, _} -> case expect_operator(rest, "=") do
      {:right, ({:tuple, _, rest_prime})} ->     case parse_pattern(rest_prime) do
      {:left, err} -> {:left, err}
      {:right, {:tuple, pat, rest_prime_prime}} ->
        success(({:tuple, label, pat}), rest_prime_prime)
    end
      {:left, _} -> success(({:tuple, label, (Nova.Compiler.Ast.pat_var(label))}), rest)
    end
end
  end
  end



  def parse_paren_pattern(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "(") do
         case parse_pattern((Nova.Array.drop(1, ts))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, pat, rest}} ->
       case expect_delimiter(rest, ")") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
           success((Nova.Compiler.Ast.pat_parens(pat)), rest_prime)
       end
   end
    else
      failure("Expected parenthesized pattern")
    end
  :nothing -> failure("Expected parenthesized pattern")
end
  end



  def parse_simple_pattern(tokens) do
    parse_any([(&parse_literal_pattern/1), (&parse_record_pattern/1), (&parse_qualified_constructor_pattern_simple/1), (&parse_as_pattern/1), (&parse_var_pattern/1), (&parse_paren_pattern/1), (&parse_tuple_pattern/1), (&parse_list_pattern/1)], tokens)
  end



  def parse_qualified_constructor_pattern_simple(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if ((t.token_type == :tok_identifier) and is_capital.(t.value)) do
      case Nova.Array.head((Nova.Array.drop(1, ts))) do
        {:just, t2} -> if (((t2.token_type == :tok_operator) and t2.value) == ".") do
                  case parse_qualified_constructor_name(ts) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, name, rest}} ->
          success((Nova.Compiler.Ast.pat_con(name, [])), rest)
      end
          else
            success((Nova.Compiler.Ast.pat_con(t.value, [])), (Nova.Array.drop(1, ts)))
          end
        :nothing -> success((Nova.Compiler.Ast.pat_con(t.value, [])), (Nova.Array.drop(1, ts)))
      end
    else
      failure("Expected constructor pattern")
    end
  :nothing -> failure("Expected constructor pattern")
end
  end



  def parse_expression(tokens) do
    parse_any([(&parse_let_expression/1), (&parse_if_expression/1), (&parse_case_expression/1), (&parse_do_block/1), (&parse_lambda/1), (&parse_typed_expression/1)], tokens)
  end



  def parse_typed_expression(tokens) do
      case parse_dollar_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, expr, rest}} ->
      case Nova.Array.head(rest) do
  {:just, t} -> if (((t.token_type == :tok_operator) and t.value) == "::") do
            rest_prime = Nova.Array.drop(1, rest)
   case parse_type_atom_sequence(rest_prime) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, ty, rest_prime_prime}} ->
       success((Nova.Compiler.Ast.expr_typed(expr, ty)), rest_prime_prime)
   end
    else
      success(expr, rest)
    end
  _ -> success(expr, rest)
end
  end
  end



  def parse_type_atom_sequence(tokens) do
      case parse_type_atom(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, first, rest}} ->
      parse_type_atom_sequence_rest(first, rest)
  end
  end



  def parse_type_atom_sequence_rest(acc, tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} ->
        cond do
          (t.token_type == :tok_newline) -> success(acc, tokens)
          (((t.token_type == :tok_operator) and t.value) == "->") ->             rest = Nova.Array.drop(1, tokens)
      case parse_type_atom_sequence(rest) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, right, rest_prime}} ->
          success((Nova.Compiler.Ast.ty_expr_arrow(acc, right)), rest_prime)
      end
        end
      _ -> case parse_type_atom(tokens) do
          {:right, ({:tuple, next, rest})} -> parse_type_atom_sequence_rest((Nova.Compiler.Ast.ty_expr_app(acc, next)), rest)
          {:left, _} -> success(acc, tokens)
        end
    end
  end



  def parse_dollar_expression(tokens) do
      case parse_hash_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
            rest_prime = skip_newlines(rest)
      case Nova.Array.head(rest_prime) do
  {:just, t} -> if (((t.token_type == :tok_operator) and t.value) == "$") do
            rest_prime_prime = skip_newlines((Nova.Array.drop(1, rest_prime)))
   case parse_dollar_expression(rest_prime_prime) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, right, rest_prime_prime_prime}} ->
       success((Nova.Compiler.Ast.expr_app(left, right)), rest_prime_prime_prime)
   end
    else
      success(left, rest)
    end
  _ -> success(left, rest)
end
  end
  end



  def parse_hash_expression(tokens) do
      case parse_composition_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
      parse_hash_expression_rest(left, rest)
  end
  end



  def parse_hash_expression_rest(left, tokens) do
    
      tokens_prime = skip_newlines(tokens)
      case Nova.Array.head(tokens_prime) do
  {:just, t} -> if (((t.token_type == :tok_operator) and t.value) == "#") do
            rest = skip_newlines((Nova.Array.drop(1, tokens_prime)))
   case parse_composition_expression(rest) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, right, rest_prime}} ->
       parse_hash_expression_rest((Nova.Compiler.Ast.expr_app(right, left)), rest_prime)
   end
    else
      success(left, tokens)
    end
  _ -> success(left, tokens)
end
  end



  def parse_composition_expression(tokens) do
      case parse_logical_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
      case Nova.Array.head(rest) do
  {:just, t} -> if ((t.token_type == :tok_operator) and is_composition_op.(t.value)) do
         case parse_composition_expression((Nova.Array.drop(1, rest))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, right, rest_prime}} ->
       success((Nova.Compiler.Ast.expr_bin_op(t.value, left, right)), rest_prime)
   end
    else
      success(left, rest)
    end
  _ -> success(left, rest)
end
  end
  end



  def parse_logical_expression(tokens) do
      case parse_cons_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
      case Nova.Array.head(rest) do
  {:just, t} -> if ((t.token_type == :tok_operator) and ((((t.value == "&&") or t.value) == "||"))) do
         case parse_logical_expression((Nova.Array.drop(1, rest))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, right, rest_prime}} ->
       success((Nova.Compiler.Ast.expr_bin_op(t.value, left, right)), rest_prime)
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
  {:just, t} -> if (((t.token_type == :tok_operator) and t.value) == ":") do
         case parse_cons_expression((Nova.Array.drop(1, rest))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, right, rest_prime}} ->
       success((Nova.Compiler.Ast.expr_bin_op(":", left, right)), rest_prime)
   end
    else
      success(left, rest)
    end
  _ -> success(left, rest)
end
  end
  end



  def parse_comparison_expression(tokens) do
        tokens_prime = skip_newlines(tokens)
  case parse_additive_expression(tokens_prime) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
            rest_prime = skip_newlines(rest)
      case Nova.Array.head(rest_prime) do
  {:just, t} -> if ((t.token_type == :tok_operator) and is_comparison_op.(t.value)) do
            rest_prime_prime = skip_newlines((Nova.Array.drop(1, rest_prime)))
   case parse_comparison_expression(rest_prime_prime) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, right, rest_prime_prime_prime}} ->
       success((Nova.Compiler.Ast.expr_bin_op(t.value, left, right)), rest_prime_prime_prime)
   end
    else
      success(left, rest)
    end
  _ -> success(left, rest)
end
  end
  end



  def parse_additive_expression(tokens) do
      case parse_multiplicative_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
      case Nova.Array.head(rest) do
  {:just, t} -> if ((t.token_type == :tok_operator) and is_additive_op.(t.value)) do
         case parse_additive_expression((Nova.Array.drop(1, rest))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, right, rest_prime}} ->
       success((Nova.Compiler.Ast.expr_bin_op(t.value, left, right)), rest_prime)
   end
    else
      success(left, rest)
    end
  _ -> success(left, rest)
end
  end
  end



  def parse_multiplicative_expression(tokens) do
      case parse_backtick_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
      case Nova.Array.head(rest) do
  {:just, t} -> if ((t.token_type == :tok_operator) and is_mult_op.(t.value)) do
         case parse_multiplicative_expression((Nova.Array.drop(1, rest))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, right, rest_prime}} ->
       success((Nova.Compiler.Ast.expr_bin_op(t.value, left, right)), rest_prime)
   end
    else
      success(left, rest)
    end
  _ -> success(left, rest)
end
  end
  end



  def parse_backtick_expression(tokens) do
      case parse_unary_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, left, rest}} ->
      parse_backtick_rest.(left).(rest)
  end
  end



  def parse_unary_expression(tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if ((t.token_type == :tok_operator) and is_unary_op.(t.value)) do
               case parse_unary_expression((Nova.Array.drop(1, tokens))) do
       {:left, err} -> {:left, err}
       {:right, {:tuple, expr, rest}} ->
         success((Nova.Compiler.Ast.expr_unary_op(t.value, expr)), rest)
     end
        else
          parse_application(tokens)
        end
      _ -> parse_application(tokens)
    end
  end



  def parse_application(tokens) do
    case Nova.Array.head(tokens) do
      {:just, first_tok} ->     case parse_term(tokens) do
      {:left, err} -> {:left, err}
      {:right, {:tuple, fn_, rest}} ->
        case maybe_parse_record_access.(fn_).(rest) do
          {:left, err} -> {:left, err}
          {:right, {:tuple, fn_prime, rest_prime}} ->
            case maybe_parse_record_update(fn_prime, rest_prime) do
              {:left, err} -> {:left, err}
              {:right, {:tuple, fn_prime_prime, rest_prime_prime}} ->
                                {:tuple, args, rest_prime_prime_prime} = collect_application_args(rest_prime_prime, [], first_tok.column)
                case Nova.Array.length(args) do
  0 -> success(fn_prime_prime, rest_prime_prime_prime)
  _ -> success((fold_app.(fn_prime_prime).(args)), rest_prime_prime_prime)
end
            end
        end
    end
      :nothing -> failure("No tokens remaining")
    end
  end



  def maybe_parse_record_update(expr, tokens) do
    
      tokens_prime = skip_newlines(tokens)
      case Nova.Array.head(tokens_prime) do
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "{") do
      case is_record_update.((Nova.Array.drop(1, tokens_prime))) do
        true ->      case parse_record_update_fields((Nova.Array.drop(1, tokens_prime))) do
       {:left, err} -> {:left, err}
       {:right, {:tuple, updates, rest}} ->
         case expect_delimiter(rest, "}") do
           {:left, err} -> {:left, err}
           {:right, {:tuple, _, rest_prime}} ->
             maybe_parse_record_update((Nova.Compiler.Ast.expr_record_update(expr, (List.from_foldable(updates)))), rest_prime)
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
    parse_separated((&parse_record_update_field/1), (fn t -> expect_delimiter(t, ",") end), tokens)
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
              success(({:tuple, label, expr}), rest_prime_prime)
          end
      end
  end
  end



  def collect_application_args(tokens, acc, base) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if (t.token_type == :tok_newline) do
          
            rest = skip_newlines((Nova.Array.drop(1, tokens)))
            case Nova.Array.head(rest) do
  {:just, t_prime} -> if looks_like_binding.(rest) do
      {:tuple, acc, rest}
    else
      if ((t_prime.column > 1) and is_continuation_token.(t_prime)) do
        case parse_term(rest) do
          {:right, ({:tuple, arg, rest_prime})} -> collect_application_args(rest_prime, (Nova.Array.snoc(acc, arg)), base)
          {:left, _} -> {:tuple, acc, rest}
        end
      else
        if (((t_prime.column > 1) and t_prime.column) > base) do
          case parse_term(rest) do
            {:right, ({:tuple, arg, rest_prime})} -> collect_application_args(rest_prime, (Nova.Array.snoc(acc, arg)), base)
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
            {:right, ({:tuple, arg, rest})} -> collect_application_args(rest, (Nova.Array.snoc(acc, arg)), base)
            {:left, _} -> {:tuple, acc, tokens}
          end
        end
      _ -> case parse_term(tokens) do
          {:right, ({:tuple, arg, rest})} -> collect_application_args(rest, (Nova.Array.snoc(acc, arg)), base)
          {:left, _} -> {:tuple, acc, tokens}
        end
    end
  end



  def parse_term(tokens) do
    parse_any([(&parse_record_literal/1), (&parse_expr_literal/1), (&parse_list_literal/1), (&parse_paren_expr/1), (&parse_tuple_literal/1), (&parse_qualified_identifier/1)], tokens)
  end



  def parse_expr_literal(tokens) do
      case parse_literal(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, lit, rest}} ->
      success((Nova.Compiler.Ast.expr_lit(lit)), rest)
  end
  end



  def parse_paren_expr(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "(") do
      
        inner = skip_newlines((Nova.Array.drop(1, ts)))
        case Nova.Array.head(inner) do
  {:just, op_tok} ->
    if ((is_operator_token(op_tok) and op_tok.value) != "-") do
      
      after_op = skip_newlines((Nova.Array.drop(1, inner)))
      case Nova.Array.head(after_op) do
  {:just, close_tok} ->
    if (((close_tok.token_type == :tok_delimiter) and close_tok.value) == ")") do
      success((Nova.Compiler.Ast.expr_section(op_tok.value)), (Nova.Array.drop(1, after_op)))
    end
  _ ->   case parse_expression((Nova.Array.drop(1, inner))) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, expr, rest}} ->
      case expect_delimiter(rest, ")") do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, rest_prime}} ->
          success((Nova.Compiler.Ast.expr_section_right(op_tok.value, expr)), rest_prime)
      end
  end
end
    end
  _ -> case parse_application(inner) do
      {:right, ({:tuple, expr, rest})} -> 
          rest_prime = skip_newlines(rest)
          case Nova.Array.head(rest_prime) do
  {:just, op_tok} ->
    if is_operator_token(op_tok) do
      
      after_op = skip_newlines((Nova.Array.drop(1, rest_prime)))
      case Nova.Array.head(after_op) do
  {:just, close_t} ->
    if (((close_t.token_type == :tok_delimiter) and close_t.value) == ")") do
      success((Nova.Compiler.Ast.expr_section_left(expr, op_tok.value)), (Nova.Array.drop(1, after_op)))
    end
  _ ->   case parse_expression(inner) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, e, r}} ->
      case expect_delimiter(r, ")") do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, r_prime}} ->
          success(e, r_prime)
      end
  end
end
    end
  _ ->   case parse_expression(inner) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, e, r}} ->
      case expect_delimiter(r, ")") do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, r_prime}} ->
          success(e, r_prime)
      end
  end
end
      {:left, _} ->     case parse_expression(inner) do
      {:left, err} -> {:left, err}
      {:right, {:tuple, e, r}} ->
        case expect_delimiter(r, ")") do
          {:left, err} -> {:left, err}
          {:right, {:tuple, _, r_prime}} ->
            success(e, r_prime)
        end
    end
    end
end
    else
      failure("Expected parenthesized expression")
    end
  :nothing -> failure("Expected parenthesized expression")
end
  end



  def is_operator_token(tok) do
    (((((((((((((((tok.token_type == :tok_operator) and tok.value) != "=") and tok.value) != "|") and tok.value) != "\\") and tok.value) != "->") and tok.value) != "<-") and tok.value) != "::") and tok.value) != "=>")
  end



  def parse_record_literal(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "{") do
         case parse_separated((&parse_record_field_expr/1), (fn tok -> expect_delimiter(tok, ",") end), (Nova.Array.drop(1, ts))) do
     {:left, err} -> {:left, err}
     {:right, {:tuple, fields, rest}} ->
       case expect_delimiter(rest, "}") do
         {:left, err} -> {:left, err}
         {:right, {:tuple, _, rest_prime}} ->
           success((Nova.Compiler.Ast.expr_record((List.from_foldable(fields)))), rest_prime)
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
  {:right, ({:tuple, _, rest_prime})} ->     rest_prime_prime = skip_newlines(rest_prime)
  case parse_expression(rest_prime_prime) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, expr, rest_prime_prime_prime}} ->
      success(({:tuple, label, expr}), rest_prime_prime_prime)
  end
  {:left, _} -> success(({:tuple, label, (Nova.Compiler.Ast.expr_var(label))}), rest)
end
  end
  end



  def parse_list_literal(tokens) do
    
      ts = skip_newlines(tokens)
      case Nova.Array.head(ts) do
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "[") do
      case Nova.Array.head((Nova.Array.drop(1, ts))) do
        {:just, t_prime} -> if (((t_prime.token_type == :tok_delimiter) and t_prime.value) == "]") do
            success((Nova.Compiler.Ast.expr_list([])), (Nova.Array.drop(2, ts)))
          else
                  case parse_separated((&parse_expression/1), (fn tok -> expect_delimiter(tok, ",") end), (Nova.Array.drop(1, ts))) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, elements, rest}} ->
          case expect_delimiter(rest, "]") do
            {:left, err} -> {:left, err}
            {:right, {:tuple, _, rest_prime}} ->
              success((Nova.Compiler.Ast.expr_list((List.from_foldable(elements)))), rest_prime)
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
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == "(") do
         case parse_separated((&parse_expression/1), (fn tok -> expect_delimiter(tok, ",") end), (Nova.Array.drop(1, ts))) do
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
  _ -> success((Nova.Compiler.Ast.expr_tuple((List.from_foldable(elements)))), rest_prime)
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
      case collect_let_bindings.(rest_prime).([]) do
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
                  success((Nova.Compiler.Ast.expr_let((List.from_foldable(bindings)), body)), rest6)
              end
          end
      end
  end
  end



  def parse_binding(tokens) do
        tokens_prime = skip_newlines(tokens)
  case parse_function_style_binding(tokens_prime) do
  {:right, r} -> {:right, r}
  {:left, _} ->   case parse_pattern(tokens_prime) do
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
    case Nova.Array.head(tokens) do
      {:just, t} -> if ((t.token_type == :tok_identifier) and not((is_upper_case.(t.value)))) do
          
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
              lambda = Nova.Compiler.Ast.expr_lambda((List.from_foldable(params)), body)
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
                          success((Nova.Compiler.Ast.expr_if(cond_, then_branch, else_branch)), rest5)
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
  {:just, first_tok} ->   case parse_case_clauses_at(rest4, first_tok.column, []) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, clauses, rest5}} ->
      success((Nova.Compiler.Ast.expr_case(expr, (List.from_foldable(clauses)))), rest5)
  end
end
          end
      end
  end
  end



  def parse_case_clauses_at(tokens, indent, acc) do
    
      tokens_prime = skip_newlines(tokens)
      case Nova.Array.head(tokens_prime) do
  :nothing -> if (Nova.Array.length(acc) > 0) do
      success(acc, tokens_prime)
    else
      failure("Expected case clause")
    end
  {:just, t} -> if (((((t.token_type == :tok_operator) and t.value) == "|") and Nova.Array.length(acc)) > 0) do
      case Nova.Array.last(acc) do
        {:just, prev_clause} -> parse_additional_guard.(tokens_prime).(prev_clause.pattern).(indent).(acc)
        :nothing -> failure("Internal error: no previous clause")
      end
    else
      if (((t.column != indent) and Nova.Array.length(acc)) > 0) do
        success(acc, tokens_prime)
      else
        if (t.column != indent) do
          failure("Case clause at wrong indentation")
        else
          case parse_case_clause(tokens_prime) do
            {:right, ({:tuple, clause, rest})} -> 
                rest_prime = skip_newlines(rest)
                case Nova.Array.head(rest_prime) do
  {:just, t2} -> if (((t2.token_type == :tok_operator) and t2.value) == "|") do
      parse_additional_guard.(rest_prime).(clause.pattern).(indent).((Nova.Array.snoc(acc, clause)))
    else
      parse_case_clauses_at(rest, indent, (Nova.Array.snoc(acc, clause)))
    end
  _ -> parse_case_clauses_at(rest, indent, (Nova.Array.snoc(acc, clause)))
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
  [] -> success(%{pattern: pat, guard: guard, body: body}, (drop_newlines(rest_prime_prime_prime)))
  _ -> case has_more_guards.(remaining) do
      true -> success(%{pattern: pat, guard: guard, body: body}, (Nova.Runtime.append(remaining, rest_prime_prime_prime)))
      false -> case is_closing_delimiter.(remaining) do
          true -> success(%{pattern: pat, guard: guard, body: body}, (Nova.Runtime.append(remaining, rest_prime_prime_prime)))
          false -> failure("Unexpected tokens after case-clause body")
        end
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
  {:just, t} -> if (((t.token_type == :tok_operator) and t.value) == "|") do
      case parse_guard_expression((Nova.Array.drop(1, tokens_prime))) do
        {:right, ({:tuple, guard, rest})} -> {:tuple, ({:just, guard}), rest}
        {:left, _} -> {:tuple, :nothing, tokens}
      end
    else
      {:tuple, :nothing, tokens}
    end
  _ -> {:tuple, :nothing, tokens}
end
  end



  def parse_guard_expression(tokens) do
      case parse_guard_parts(tokens, []) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, guards, rest}} ->
      case Nova.Array.length(guards) do
  0 -> failure("Expected guard expression")
  1 -> case Nova.Array.head(guards) do
      {:just, g} -> success(g, rest)
      :nothing -> failure("No guard")
    end
  _ -> success((fold_guards.(guards)), rest)
end
  end
  end



  def parse_guard_parts(tokens, acc) do
      case parse_guard_part(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, g, rest}} ->
            rest_prime = skip_newlines(rest)
      case Nova.Array.head(rest_prime) do
  {:just, t} -> if (((t.token_type == :tok_delimiter) and t.value) == ",") do
      parse_guard_parts((Nova.Array.drop(1, rest_prime)), (Nova.Array.snoc(acc, g)))
    else
      success((Nova.Array.snoc(acc, g)), rest_prime)
    end
  _ -> success((Nova.Array.snoc(acc, g)), rest_prime)
end
  end
  end



  def parse_guard_part(tokens) do
        tokens_prime = skip_newlines(tokens)
  case try_pattern_bind.(tokens_prime) do
  {:right, result} -> {:right, result}
  {:left, _} -> parse_logical_expression(tokens_prime)
end
  end



  def take_body(tokens, acc, indent) do
    case Nova.Array.head(tokens) do
      :nothing -> {:tuple, (Nova.Array.reverse(acc)), []}
      {:just, t} -> if (t.token_type == :tok_newline) do
          
            rest = skip_newlines((Nova.Array.drop(1, tokens)))
            case Nova.Array.head(rest) do
  {:just, t_prime} -> if (t_prime.column < indent) do
      {:tuple, (Nova.Array.reverse(acc)), tokens}
    else
      if ((t_prime.column == indent) and clause_start(rest)) do
        {:tuple, (Nova.Array.reverse(acc)), tokens}
      else
        if ((((t_prime.token_type == :tok_operator) and t_prime.value) == "|") and guard_start(rest)) do
          {:tuple, (Nova.Array.reverse(acc)), tokens}
        else
          if (((t_prime.token_type == :tok_keyword) and t_prime.value) == "where") do
            {:tuple, (Nova.Array.reverse(acc)), tokens}
          else
            take_body(rest, (Nova.Array.cons(t, acc)), indent)
          end
        end
      end
    end
  _ -> take_body(rest, (Nova.Array.cons(t, acc)), indent)
end
        else
          take_body((Nova.Array.drop(1, tokens)), (Nova.Array.cons(t, acc)), indent)
        end
    end
  end



  def guard_start(tokens) do
    case Nova.Array.head(tokens) do
      {:just, t} -> if (((t.token_type == :tok_operator) and t.value) == "|") do
          case parse_guard_expression((Nova.Array.drop(1, tokens))) do
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
  :nothing -> success((Nova.Compiler.Ast.expr_do([])), rest_prime)
  {:just, first_tok} ->     indent = first_tok.column
  case parse_do_statements_at(rest_prime, indent, []) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, stmts, rest_prime_prime}} ->
      success((Nova.Compiler.Ast.expr_do((List.from_foldable(stmts)))), rest_prime_prime)
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
      if (((t.column > indent) and Nova.Array.length(acc)) == 0) do
        case parse_do_statement(tokens_prime) do
          {:right, ({:tuple, stmt, rest})} -> parse_do_statements_at(rest, indent, (Nova.Array.snoc(acc, stmt)))
          {:left, _} -> success(acc, tokens_prime)
        end
      else
        if (((t.column != indent) and Nova.Array.length(acc)) > 0) do
          success(acc, tokens_prime)
        else
          case parse_do_statement(tokens_prime) do
            {:right, ({:tuple, stmt, rest})} -> parse_do_statements_at(rest, indent, (Nova.Array.snoc(acc, stmt)))
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
          success((Nova.Compiler.Ast.do_let((List.from_foldable(bindings)))), rest_prime)
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
              success((Nova.Compiler.Ast.do_bind(pat, expr)), rest_prime_prime)
          end
      end
  end
  end



  def parse_do_expr(tokens) do
      case parse_expression(tokens) do
    {:left, err} -> {:left, err}
    {:right, {:tuple, expr, rest}} ->
      success((Nova.Compiler.Ast.do_expr(expr)), rest)
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
                  success((Nova.Compiler.Ast.expr_lambda((List.from_foldable(params)), body)), rest_prime_prime_prime)
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
    parse_any([(&parse_module_header/1), (&parse_import/1), parse_foreign_import_simple, parse_infix_declaration, parse_newtype_declaration, parse_data_declaration, parse_type_alias, parse_type_class, parse_type_class_instance, parse_function_with_type_signature, parse_function_declaration, parse_type_signature_decl], tokens)
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
              success((Nova.Compiler.Ast.decl_module(%{name: name, declarations: []})), rest_prime_prime)
          end
      end
  end
  end



  def parse_qualified_identifier_name(tokens) do
    case parse_separated((&parse_identifier_name/1), (fn t -> expect_operator(t, ".") end), tokens) do
      {:left, err} -> {:left, err}
      {:right, ({:tuple, parts, rest})} -> success((Nova.String.join_with(".", parts)), rest)
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
                    {:tuple, alias_, rest_prime_prime} = parse_import_alias.(rest_prime)
          case parse_import_selectors.(rest_prime_prime) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, result, rest_prime_prime_prime}} ->
nil
          end
      end
  end
  end
end
