defmodule Nova.Compiler.CstParser do
  # import Prelude

  # import Data.Array

  # import Data.List

  # import Data.List

  # import Data.String

  # import Data.Maybe

  # import Data.Either

  # import Data.Tuple

  # import Control.Lazy

  # import Nova.Compiler.Cst

  # @type token_stream :: list()(source_token())

  # @type parse_result :: either()(string())((tuple()(a)(token_stream())))

  # Newtype: Parser
  def parser(arg0), do: {:parser, arg0}



  def run_parser(({:parser, p})) do
    p
  end

  # instance Functor parser()
  def map(f, ({:parser, p})) do
    {:parser, fn ts -> case p.(ts) do
      {:left, err} -> {:left, err}
      {:right, ({:tuple, a, rest})} -> {:right, ({:tuple, (f.(a)), rest})}
    end end}
  end

  # instance Apply parser()
  def apply(({:parser, pf}), ({:parser, pa})) do
    {:parser, fn ts -> case pf.(ts) do
      {:left, err} -> {:left, err}
      {:right, ({:tuple, f, rest})} -> case pa.(rest) do
          {:left, err} -> {:left, err}
          {:right, ({:tuple, a, rest_prime})} -> {:right, ({:tuple, (f.(a)), rest_prime})}
        end
    end end}
  end

  # instance Applicative parser()
  def pure(a) do
    {:parser, fn ts -> {:right, ({:tuple, a, ts})} end}
  end

  # instance Bind parser()
  def bind(({:parser, pa}), f) do
    {:parser, fn ts -> case pa.(ts) do
      {:left, err} -> {:left, err}
      {:right, ({:tuple, a, rest})} -> run_parser((f.(a))).(rest)
    end end}
  end

  # instance Monad parser()

  # instance Lazy (parser()(a))
  def defer(f) do
    {:parser, fn ts -> run_parser((f.(:unit))).(ts) end}
  end

  # infixl 3 alt as <|>



  def alt(({:parser, p1}), ({:parser, p2})) do
    {:parser, fn ts -> case p1.(ts) do
      {:right, result} -> {:right, result}
      {:left, _} -> p2.(ts)
    end end}
  end



  def fail(msg) do
    {:parser, fn _ -> {:left, msg} end}
  end



  def get_position() do
    {:parser, fn ts -> case ts do
      ([tok | _]) -> {:right, ({:tuple, tok.range.start, ts})}
      [] -> {:right, ({:tuple, %{line: 0, column: 0}, ts})}
    end end}
  end



  def satisfy(pred) do
    {:parser, fn ts -> case ts do
      ([tok | rest]) -> if pred.(tok.value) do
          {:right, ({:tuple, tok, rest})}
        else
          {:left, "Token did not match predicate"}
        end
      [] -> {:left, "Unexpected end of input"}
    end end}
  end



  def token(expected) do
    satisfy((fn x__ -> (x__ == expected) end))
  end



  def expect_map(f) do
    {:parser, fn ts -> case ts do
      ([tok | rest]) -> case f.(tok.value) do
          {:just, a} -> {:right, ({:tuple, ({:tuple, tok, a}), rest})}
          :nothing -> {:left, "Token did not match"}
        end
      [] -> {:left, "Unexpected end of input"}
    end end}
  end



  def tok_left_paren() do
    token(Nova.Compiler.Cst.tok_left_paren())
  end



  def tok_right_paren() do
    token(Nova.Compiler.Cst.tok_right_paren())
  end



  def tok_left_brace() do
    token(Nova.Compiler.Cst.tok_left_brace())
  end



  def tok_right_brace() do
    token(Nova.Compiler.Cst.tok_right_brace())
  end



  def tok_left_square() do
    token(Nova.Compiler.Cst.tok_left_square())
  end



  def tok_right_square() do
    token(Nova.Compiler.Cst.tok_right_square())
  end



  def tok_equals() do
    token(Nova.Compiler.Cst.tok_equals())
  end



  def tok_pipe() do
    token(Nova.Compiler.Cst.tok_pipe())
  end



  def tok_dot() do
    token(Nova.Compiler.Cst.tok_dot())
  end



  def tok_double_dot() do
    
      is_double_dot = fn auto_arg0 -> case auto_arg0 do
        ({:tok_operator, _, ".."}) -> true
        _ -> false
      end end
      satisfy(is_double_dot)
  end



  def tok_comma() do
    token(Nova.Compiler.Cst.tok_comma())
  end



  def tok_backslash() do
    token(Nova.Compiler.Cst.tok_backslash())
  end



  def tok_right_arrow() do
    token(Nova.Compiler.Cst.tok_right_arrow())
  end



  def tok_left_arrow() do
    token(Nova.Compiler.Cst.tok_left_arrow())
  end



  def tok_double_colon() do
    token(Nova.Compiler.Cst.tok_double_colon())
  end



  def tok_colon() do
    
      is_colon = fn auto_arg0 -> case auto_arg0 do
        ({:tok_operator, _, ":"}) -> true
        _ -> false
      end end
      satisfy(is_colon)
  end



  def tok_right_fat_arrow() do
    token(Nova.Compiler.Cst.tok_right_fat_arrow())
  end



  def tok_forall() do
    token(Nova.Compiler.Cst.tok_forall())
  end



  def tok_tick() do
    token(Nova.Compiler.Cst.tok_tick())
  end



  def tok_layout_start() do
    
      is_layout_start = fn auto_arg0 -> case auto_arg0 do
        ({:tok_layout_start, _}) -> true
        _ -> false
      end end
      satisfy(is_layout_start)
  end



  def tok_layout_sep() do
    
      is_layout_sep = fn auto_arg0 -> case auto_arg0 do
        ({:tok_layout_sep, _}) -> true
        _ -> false
      end end
      satisfy(is_layout_sep)
  end



  def tok_layout_end() do
    
      is_layout_end = fn auto_arg0 -> case auto_arg0 do
        ({:tok_layout_end, _}) -> true
        _ -> false
      end end
      satisfy(is_layout_end)
  end



  def tok_lower_name() do
    
      reserved_keywords = ["module" | ["where" | ["import" | ["data" | ["type" | ["newtype" | ["class" | ["instance" | ["derive" | ["foreign" | ["infixl" | ["infixr" | ["infix" | ["if" | ["then" | ["else" | ["case" | ["of" | ["let" | ["in" | ["do" | ["ado" | ["forall" | ["as" | ["hiding" | ["true" | ["false" | []]]]]]]]]]]]]]]]]]]]]]]]]]]]
      is_keyword = fn s -> Nova.List.elem(s, reserved_keywords) end
      extract_lower = fn auto_arg0 -> case auto_arg0 do
        ({:tok_lower_name, :nothing, name}) -> if is_keyword.(name) do
  :nothing
else
  {:just, name}
end
        _ -> :nothing
      end end
      Nova.Runtime.bind(expect_map(extract_lower), fn {:tuple, tok, name} ->
  Nova.Runtime.pure(%{token: tok, name: Nova.Compiler.Cst.ident(name)})
end)
  end



  def tok_qualified_lower_name() do
    
      reserved_keywords = ["module" | ["where" | ["import" | ["data" | ["type" | ["newtype" | ["class" | ["instance" | ["derive" | ["foreign" | ["infixl" | ["infixr" | ["infix" | ["if" | ["then" | ["else" | ["case" | ["of" | ["let" | ["in" | ["do" | ["ado" | ["forall" | ["as" | ["hiding" | ["true" | ["false" | []]]]]]]]]]]]]]]]]]]]]]]]]]]]
      is_keyword = fn s -> Nova.List.elem(s, reserved_keywords) end
      extract_qual_lower = fn auto_arg0 -> case auto_arg0 do
        ({:tok_lower_name, mod_, name}) -> if is_keyword.(name) do
  :nothing
else
  {:just, ({:tuple, (Nova.Runtime.map((&Nova.Compiler.Cst.module_name/1), mod_)), name})}
end
        _ -> :nothing
      end end
      Nova.Runtime.bind(expect_map(extract_qual_lower), fn {:tuple, tok, ({:tuple, mod_, name})} ->
  Nova.Runtime.pure(%{token: tok, module: mod_, name: Nova.Compiler.Cst.ident(name)})
end)
  end



  def tok_upper_name() do
    
      extract_upper = fn auto_arg0 -> case auto_arg0 do
        ({:tok_upper_name, :nothing, name}) -> {:just, name}
        _ -> :nothing
      end end
      Nova.Runtime.bind(expect_map(extract_upper), fn {:tuple, tok, name} ->
  Nova.Runtime.pure(%{token: tok, name: Nova.Compiler.Cst.proper(name)})
end)
  end



  def tok_qualified_upper_name() do
    
      extract_qual_upper = fn auto_arg0 -> case auto_arg0 do
        ({:tok_upper_name, mod_, name}) -> {:just, ({:tuple, (Nova.Runtime.map((&Nova.Compiler.Cst.module_name/1), mod_)), name})}
        _ -> :nothing
      end end
      Nova.Runtime.bind(expect_map(extract_qual_upper), fn {:tuple, tok, ({:tuple, mod_, name})} ->
  Nova.Runtime.pure(%{token: tok, module: mod_, name: Nova.Compiler.Cst.proper(name)})
end)
  end



  def tok_operator() do
    
      extract_op = fn auto_arg0 -> case auto_arg0 do
        ({:tok_operator, mod_, op}) -> {:just, ({:tuple, (Nova.Runtime.map((&Nova.Compiler.Cst.module_name/1), mod_)), op})}
        _ -> :nothing
      end end
      Nova.Runtime.bind(expect_map(extract_op), fn {:tuple, tok, ({:tuple, mod_, op})} ->
  Nova.Runtime.pure(%{token: tok, module: mod_, name: Nova.Compiler.Cst.operator(op)})
end)
  end



  def tok_keyword(kw) do
    
      is_kw = fn auto_arg0 -> case auto_arg0 do
        ({:tok_lower_name, :nothing, name}) -> (name == kw)
        _ -> false
      end end
      satisfy(is_kw)
  end



  def tok_string() do
    
      extract_str = fn auto_arg0 -> case auto_arg0 do
        ({:tok_string, _, s}) -> {:just, s}
        _ -> :nothing
      end end
      expect_map(extract_str)
  end



  def tok_hole() do
    
      extract_hole = fn auto_arg0 -> case auto_arg0 do
        ({:tok_hole, name}) -> {:just, name}
        _ -> :nothing
      end end
      expect_map(extract_hole)
  end



  def tok_int() do
    
      extract_int = fn auto_arg0 -> case auto_arg0 do
        ({:tok_int, _, n}) -> {:just, n}
        _ -> :nothing
      end end
      expect_map(extract_int)
  end



  def tok_number() do
    
      extract_num = fn auto_arg0 -> case auto_arg0 do
        ({:tok_number, _, n}) -> {:just, n}
        _ -> :nothing
      end end
      expect_map(extract_num)
  end



  def tok_char() do
    
      extract_char = fn auto_arg0 -> case auto_arg0 do
        ({:tok_char, _, c}) -> {:just, c}
        _ -> :nothing
      end end
      expect_map(extract_char)
  end



  def optional(p) do
    Nova.Runtime.alt((Nova.Runtime.map((fn x -> {:just, x} end), p)), Nova.Runtime.pure(:nothing))
  end



  def many(p) do
    {:parser, fn ts -> many_go(p, ts, []) end}
  end



  def many_go(p, tokens, acc) do
    
      p_func = run_parser(p)
      case p_func.(tokens) do
  {:right, ({:tuple, a, rest})} -> many_go(p, rest, (Nova.Runtime.append(acc, ([a | []]))))
  {:left, _} -> {:right, ({:tuple, acc, tokens})}
end
  end



  def some(p) do
      Nova.Runtime.bind(p, fn first ->
    Nova.Runtime.bind(many(p), fn rest ->
      Nova.Runtime.pure(([first | rest]))
    end)
  end)
  end



  def separated(p, sep) do
      Nova.Runtime.bind(p, fn head ->
    Nova.Runtime.bind(many((Nova.Runtime.bind(sep, fn s ->
  Nova.Runtime.bind(p, fn a ->
    Nova.Runtime.pure(({:tuple, s, a}))
  end)
end))), fn tail ->
      Nova.Runtime.pure(%{head: head, tail: tail})
    end)
  end)
  end



  def optional_separated(p, sep) do
    optional((separated(p, sep)))
  end



  def wrapped(open, p, close) do
      Nova.Runtime.bind(open, fn o ->
    Nova.Runtime.bind(p, fn value ->
      Nova.Runtime.bind(close, fn c ->
        Nova.Runtime.pure(%{open: o, value: value, close: c})
      end)
    end)
  end)
  end



  def delimited(open, p, sep, close) do
    wrapped(open, (optional_separated(p, sep)), close)
  end



  def layout_block(p) do
    
      dummy_token = %{range: %{start: %{line: 0, column: 0}, end_: %{line: 0, column: 0}}, leading_comments: [], trailing_comments: [], value: Nova.Compiler.Cst.tok_layout_end(0)}
      Nova.Runtime.bind(tok_layout_start(), fn _ ->
  Nova.Runtime.bind(layout_items(p), fn items ->
    Nova.Runtime.bind(Nova.Runtime.alt(tok_layout_end(), Nova.Runtime.pure(dummy_token)), fn _ ->
      Nova.Runtime.pure(items)
    end)
  end)
end)
  end



  def layout_items(p) do
      Nova.Runtime.bind(p, fn first ->
    Nova.Runtime.bind(many((Nova.Runtime.seq(tok_layout_sep(), p))), fn rest ->
      Nova.Runtime.pure(([first | rest]))
    end)
  end)
  end



  def parse_type() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.alt(parse_type_forall(), parse_type1()) end)
  end



  def parse_type_forall() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_forall(), fn forall_tok ->
    Nova.Runtime.bind(some(parse_type_var_binding()), fn vars ->
      Nova.Runtime.bind(tok_dot(), fn dot ->
        Nova.Runtime.bind(parse_type(), fn body ->
          Nova.Runtime.pure((Nova.Compiler.Cst.type_forall(forall_tok, vars, dot, body)))
        end)
      end)
    end)
  end) end)
  end



  def parse_type1() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_type2(), fn t ->
    Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_right_fat_arrow(), fn arr ->
  Nova.Runtime.bind(parse_type1(), fn t2 ->
    Nova.Runtime.pure(({:tuple, arr, t2}))
  end)
end))), fn rest ->
      case rest do
  {:just, ({:tuple, arr, t2})} -> Nova.Runtime.pure((Nova.Compiler.Cst.type_constrained(t, arr, t2)))
  :nothing -> Nova.Runtime.pure(t)
end
    end)
  end) end)
  end



  def parse_type2() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_type3(), fn t ->
    Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_right_arrow(), fn arr ->
  Nova.Runtime.bind(parse_type2(), fn t2 ->
    Nova.Runtime.pure(({:tuple, arr, t2}))
  end)
end))), fn rest ->
      case rest do
  {:just, ({:tuple, arr, t2})} -> Nova.Runtime.pure((Nova.Compiler.Cst.type_arrow(t, arr, t2)))
  :nothing -> Nova.Runtime.pure(t)
end
    end)
  end) end)
  end



  def parse_type3() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_type_atom(), fn head ->
    Nova.Runtime.bind(many(parse_type_atom()), fn args ->
      if Nova.List.null(args) do
  Nova.Runtime.pure(head)
else
  Nova.Runtime.pure((Nova.Compiler.Cst.type_app(head, args)))
end
    end)
  end) end)
  end



  def parse_type_atom() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(parse_type_var(), parse_type_con()), parse_type_parens()), parse_type_record()) end)
  end



  def parse_type_var() do
    Nova.Runtime.fmap((&Nova.Compiler.Cst.type_var/1), tok_lower_name())
  end



  def parse_type_con() do
    Nova.Runtime.fmap((&Nova.Compiler.Cst.type_constructor/1), tok_qualified_upper_name())
  end



  def parse_type_parens() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(wrapped(tok_left_paren(), parse_type(), tok_right_paren()), fn w ->
    Nova.Runtime.pure((Nova.Compiler.Cst.type_parens(w)))
  end) end)
  end



  def parse_type_record() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(wrapped(tok_left_brace(), parse_row(), tok_right_brace()), fn w ->
    Nova.Runtime.pure((Nova.Compiler.Cst.type_record(w)))
  end) end)
  end



  def parse_row() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(optional_separated(parse_row_label(), tok_comma()), fn labels ->
    Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_pipe(), fn pipe ->
  Nova.Runtime.bind(parse_type(), fn t ->
    Nova.Runtime.pure(({:tuple, pipe, t}))
  end)
end))), fn tail ->
      Nova.Runtime.pure(%{labels: labels, tail: tail})
    end)
  end) end)
  end



  def parse_row_label() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_label(), fn label ->
    Nova.Runtime.bind(tok_double_colon(), fn sep ->
      Nova.Runtime.bind(parse_type(), fn ty ->
        Nova.Runtime.pure(%{label: label, separator: sep, value: ty})
      end)
    end)
  end) end)
  end



  def parse_label() do
    
      extract_label = fn auto_arg0 -> case auto_arg0 do
        ({:tok_lower_name, :nothing, name}) -> {:just, name}
        ({:tok_string, _, s}) -> {:just, s}
        _ -> :nothing
      end end
      Nova.Runtime.bind(expect_map(extract_label), fn {:tuple, tok, name} ->
  Nova.Runtime.pure(%{token: tok, name: Nova.Compiler.Cst.label(name)})
end)
  end



  def parse_expr() do
    Control.Lazy.defer(fn _ -> parse_expr1() end)
  end



  def parse_expr1() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_expr2(), fn e ->
    Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_double_colon(), fn dc ->
  Nova.Runtime.bind(parse_type(), fn t ->
    Nova.Runtime.pure(({:tuple, dc, t}))
  end)
end))), fn ann ->
      case ann do
  {:just, ({:tuple, dc, t})} -> Nova.Runtime.pure((Nova.Compiler.Cst.expr_typed(e, dc, t)))
  :nothing -> Nova.Runtime.pure(e)
end
    end)
  end) end)
  end



  def parse_expr2() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_expr3(), fn e ->
    parse_expr2_rest(e)
  end) end)
  end



  def parse_expr2_rest(e) do
      Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_operator(), fn op ->
  Nova.Runtime.bind(parse_expr3(), fn e2 ->
    Nova.Runtime.pure(({:tuple, op, e2}))
  end)
end))), fn op_result ->
    case op_result do
  {:just, ({:tuple, op, e2})} ->   Nova.Runtime.bind(parse_expr2_ops(([{:tuple, op, e2} | []])), fn rest ->
    Nova.Runtime.pure((Nova.Compiler.Cst.expr_op(e, rest)))
  end)
  :nothing ->   Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_tick(), fn tick1 ->
  Nova.Runtime.bind(parse_expr5(), fn func_expr ->
    Nova.Runtime.bind(tok_tick(), fn tick2 ->
      Nova.Runtime.bind(parse_expr3(), fn e2 ->
        Nova.Runtime.pure(({:tuple, %{open: tick1, value: func_expr, close: tick2}, e2}))
      end)
    end)
  end)
end))), fn tick_result ->
    case tick_result do
  {:just, ({:tuple, wrapped, e2})} ->   Nova.Runtime.bind(parse_expr2_infix(([{:tuple, wrapped, e2} | []])), fn rest ->
    Nova.Runtime.pure((Nova.Compiler.Cst.expr_infix(e, rest)))
  end)
  :nothing -> Nova.Runtime.pure(e)
end
  end)
end
  end)
  end



  def parse_expr2_ops(acc) do
      Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_operator(), fn op ->
  Nova.Runtime.bind(parse_expr3(), fn e2 ->
    Nova.Runtime.pure(({:tuple, op, e2}))
  end)
end))), fn result ->
    case result do
  {:just, item} -> parse_expr2_ops((Nova.Runtime.append(acc, ([item | []]))))
  :nothing -> Nova.Runtime.pure(acc)
end
  end)
  end



  def parse_expr2_infix(acc) do
      Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_tick(), fn tick1 ->
  Nova.Runtime.bind(parse_expr5(), fn func_expr ->
    Nova.Runtime.bind(tok_tick(), fn tick2 ->
      Nova.Runtime.bind(parse_expr3(), fn e2 ->
        Nova.Runtime.pure(({:tuple, %{open: tick1, value: func_expr, close: tick2}, e2}))
      end)
    end)
  end)
end))), fn result ->
    case result do
  {:just, item} -> parse_expr2_infix((Nova.Runtime.append(acc, ([item | []]))))
  :nothing -> Nova.Runtime.pure(acc)
end
  end)
  end



  def parse_expr3() do
    
      is_negate = fn auto_arg0 -> case auto_arg0 do
        ({:tok_operator, :nothing, "-"}) -> true
        _ -> false
      end end
      parse_negate =    Nova.Runtime.bind(satisfy(is_negate), fn neg ->
     Nova.Runtime.bind(parse_expr3(), fn e ->
       Nova.Runtime.pure((Nova.Compiler.Cst.expr_negate(neg, e)))
     end)
   end)
      Control.Lazy.defer(fn _ -> Nova.Runtime.alt(parse_negate, parse_expr4()) end)
  end



  def parse_expr4() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_expr5(), fn head ->
    Nova.Runtime.bind(many(parse_expr5()), fn args ->
      if Nova.List.null(args) do
  Nova.Runtime.pure(head)
else
  Nova.Runtime.pure((Nova.Compiler.Cst.expr_app(head, args)))
end
    end)
  end) end)
  end



  def parse_expr5() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(parse_if(), parse_let()), parse_lambda()), parse_case()), parse_do()), parse_expr_atom()), fn e ->
    parse_record_accessor(e)
  end) end)
  end



  def parse_record_accessor(e) do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(optional(parse_record_update_part()), fn update_result ->
    case update_result do
  {:just, updates} -> Nova.Runtime.pure((Nova.Compiler.Cst.expr_record_update(e, updates)))
  :nothing ->   Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_dot(), fn dot ->
  Nova.Runtime.bind(parse_label(), fn first ->
    Nova.Runtime.bind(many((Nova.Runtime.seq(tok_dot(), parse_label()))), fn rest ->
      Nova.Runtime.pure(%{dot: dot, path: %{head: first, tail: Nova.Runtime.map((fn l -> {:tuple, dot, l} end), rest)}})
    end)
  end)
end))), fn access ->
    case access do
  {:just, %{dot: dot, path: path}} ->     access_expr = Nova.Compiler.Cst.expr_record_accessor(%{expr: e, dot: dot, path: path})
  parse_record_accessor(access_expr)
  :nothing -> Nova.Runtime.pure(e)
end
  end)
end
  end) end)
  end



  def parse_record_update_part() do
      Nova.Runtime.bind(tok_left_brace(), fn open ->
    Nova.Runtime.bind(parse_record_update_field(), fn first ->
      Nova.Runtime.bind(many((Nova.Runtime.bind(tok_comma(), fn comma ->
  Nova.Runtime.bind(parse_record_update_field(), fn field ->
    Nova.Runtime.pure(({:tuple, comma, field}))
  end)
end))), fn rest ->
        Nova.Runtime.bind(tok_right_brace(), fn close ->
          Nova.Runtime.pure(%{open: open, value: %{head: first, tail: rest}, close: close})
        end)
      end)
    end)
  end)
  end



  def parse_record_update_field() do
      Nova.Runtime.bind(parse_label(), fn label ->
    Nova.Runtime.bind(tok_equals(), fn eq ->
      Nova.Runtime.bind(parse_expr(), fn value ->
        Nova.Runtime.pure((Nova.Compiler.Cst.record_update_leaf(label, eq, value)))
      end)
    end)
  end)
  end



  def parse_if() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_keyword("if"), fn kw ->
    Nova.Runtime.bind(parse_expr(), fn cond_expr ->
      Nova.Runtime.bind(tok_keyword("then"), fn then_kw ->
        Nova.Runtime.bind(parse_expr(), fn true_expr ->
          Nova.Runtime.bind(tok_keyword("else"), fn else_kw ->
            Nova.Runtime.bind(parse_expr(), fn false_expr ->
              Nova.Runtime.pure((Nova.Compiler.Cst.expr_if(%{keyword: kw, cond_: cond_expr, then_kw: then_kw, then_branch: true_expr, else_kw: else_kw, else_branch: false_expr})))
            end)
          end)
        end)
      end)
    end)
  end) end)
  end



  def parse_let() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_keyword("let"), fn kw ->
    Nova.Runtime.bind(layout_block(parse_let_binding()), fn bindings ->
      Nova.Runtime.bind(tok_keyword("in"), fn in_kw ->
        Nova.Runtime.bind(parse_expr(), fn body ->
          Nova.Runtime.pure((Nova.Compiler.Cst.expr_let(%{keyword: kw, bindings: bindings, in_: in_kw, body: body})))
        end)
      end)
    end)
  end) end)
  end



  def parse_lambda() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_backslash(), fn bs ->
    Nova.Runtime.bind(some(parse_binder()), fn binders ->
      Nova.Runtime.bind(tok_right_arrow(), fn arr ->
        Nova.Runtime.bind(parse_expr(), fn body ->
          Nova.Runtime.pure((Nova.Compiler.Cst.expr_lambda(%{symbol: bs, binders: binders, arrow: arr, body: body})))
        end)
      end)
    end)
  end) end)
  end



  def parse_case() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_keyword("case"), fn kw ->
    Nova.Runtime.bind(separated(parse_expr(), tok_comma()), fn head ->
      Nova.Runtime.bind(tok_keyword("of"), fn of_kw ->
        Nova.Runtime.bind(layout_block(parse_case_branch()), fn branches ->
          Nova.Runtime.pure((Nova.Compiler.Cst.expr_case(%{keyword: kw, head: head, of: of_kw, branches: branches})))
        end)
      end)
    end)
  end) end)
  end



  def parse_case_branch() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(separated(parse_binder(), tok_comma()), fn pats ->
    Nova.Runtime.bind(parse_guarded_arrow(), fn guarded ->
      Nova.Runtime.pure(({:tuple, pats, guarded}))
    end)
  end) end)
  end



  def parse_guarded_arrow() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.alt(parse_unconditional_arrow(), parse_guarded_exprs_arrow()) end)
  end



  def parse_unconditional_arrow() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_right_arrow(), fn arrow ->
    Nova.Runtime.bind(parse_expr(), fn expr ->
      Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_keyword("where"), fn wh ->
  Nova.Runtime.bind(layout_block(parse_let_binding()), fn bindings ->
    Nova.Runtime.pure(({:tuple, wh, bindings}))
  end)
end))), fn where_clause ->
        Nova.Runtime.pure((Nova.Compiler.Cst.unconditional(arrow, %{expr: expr, bindings: where_clause})))
      end)
    end)
  end) end)
  end



  def parse_guarded_exprs_arrow() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(some(parse_guarded_expr_arrow()), fn guards ->
    Nova.Runtime.pure((Nova.Compiler.Cst.guarded(guards)))
  end) end)
  end



  def parse_guarded_expr_arrow() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_pipe(), fn bar ->
    Nova.Runtime.bind(separated(parse_pattern_guard(), tok_comma()), fn patterns ->
      Nova.Runtime.bind(tok_right_arrow(), fn sep ->
        Nova.Runtime.bind(parse_expr(), fn expr ->
          Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_keyword("where"), fn wh ->
  Nova.Runtime.bind(layout_block(parse_let_binding()), fn bindings ->
    Nova.Runtime.pure(({:tuple, wh, bindings}))
  end)
end))), fn where_clause ->
            Nova.Runtime.pure(%{bar: bar, patterns: patterns, separator: sep, where: %{expr: expr, bindings: where_clause}})
          end)
        end)
      end)
    end)
  end) end)
  end



  def parse_do() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_keyword("do"), fn kw ->
    Nova.Runtime.bind(layout_block(parse_do_statement()), fn stmts ->
      Nova.Runtime.pure((Nova.Compiler.Cst.expr_do(%{keyword: kw, statements: stmts})))
    end)
  end) end)
  end



  def parse_do_statement() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.alt(Nova.Runtime.alt(parse_do_let(), parse_do_bind()), parse_do_discard()) end)
  end



  def parse_do_let() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_keyword("let"), fn kw ->
    Nova.Runtime.bind(layout_block(parse_let_binding()), fn bindings ->
      Nova.Runtime.pure((Nova.Compiler.Cst.do_let(kw, bindings)))
    end)
  end) end)
  end



  def parse_do_bind() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_binder(), fn binder ->
    Nova.Runtime.bind(tok_left_arrow(), fn arr ->
      Nova.Runtime.bind(parse_expr(), fn expr ->
        Nova.Runtime.pure((Nova.Compiler.Cst.do_bind(binder, arr, expr)))
      end)
    end)
  end) end)
  end



  def parse_do_discard() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.fmap((&Nova.Compiler.Cst.do_discard/1), parse_expr()) end)
  end



  def parse_let_binding() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.alt(Nova.Runtime.alt(parse_let_sig(), parse_let_pattern()), parse_let_name()) end)
  end



  def parse_let_sig() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_lower_name(), fn name ->
    Nova.Runtime.bind(tok_double_colon(), fn dc ->
      Nova.Runtime.bind(parse_type(), fn ty ->
        Nova.Runtime.pure((Nova.Compiler.Cst.let_binding_signature(%{label: name, separator: dc, value: ty})))
      end)
    end)
  end) end)
  end



  def parse_let_pattern() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(Nova.Runtime.alt(parse_binder_con(), parse_binder_record()), fn pat ->
    Nova.Runtime.bind(tok_equals(), fn eq ->
      Nova.Runtime.bind(parse_expr(), fn expr ->
        Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_keyword("where"), fn wh ->
  Nova.Runtime.bind(layout_block(parse_let_binding()), fn bindings ->
    Nova.Runtime.pure(({:tuple, wh, bindings}))
  end)
end))), fn where_clause ->
          Nova.Runtime.pure((Nova.Compiler.Cst.let_binding_pattern(pat, eq, %{expr: expr, bindings: where_clause})))
        end)
      end)
    end)
  end) end)
  end



  def parse_let_name() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_lower_name(), fn name ->
    Nova.Runtime.bind(many(parse_binder_atom()), fn binders ->
      Nova.Runtime.bind(parse_guarded(), fn guarded ->
        Nova.Runtime.pure((Nova.Compiler.Cst.let_binding_name(%{name: name, binders: binders, guarded: guarded})))
      end)
    end)
  end) end)
  end



  def parse_guarded() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.alt(parse_unconditional(), parse_guarded_exprs()) end)
  end



  def parse_unconditional() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_equals(), fn eq ->
    Nova.Runtime.bind(parse_expr(), fn expr ->
      Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_keyword("where"), fn wh ->
  Nova.Runtime.bind(layout_block(parse_let_binding()), fn bindings ->
    Nova.Runtime.pure(({:tuple, wh, bindings}))
  end)
end))), fn where_clause ->
        Nova.Runtime.pure((Nova.Compiler.Cst.unconditional(eq, %{expr: expr, bindings: where_clause})))
      end)
    end)
  end) end)
  end



  def parse_guarded_exprs() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(some(parse_guarded_expr()), fn guards ->
    Nova.Runtime.pure((Nova.Compiler.Cst.guarded(guards)))
  end) end)
  end



  def parse_guarded_expr() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_pipe(), fn bar ->
    Nova.Runtime.bind(separated(parse_pattern_guard(), tok_comma()), fn patterns ->
      Nova.Runtime.bind(tok_equals(), fn sep ->
        Nova.Runtime.bind(parse_expr(), fn expr ->
          Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_keyword("where"), fn wh ->
  Nova.Runtime.bind(layout_block(parse_let_binding()), fn bindings ->
    Nova.Runtime.pure(({:tuple, wh, bindings}))
  end)
end))), fn where_clause ->
            Nova.Runtime.pure(%{bar: bar, patterns: patterns, separator: sep, where: %{expr: expr, bindings: where_clause}})
          end)
        end)
      end)
    end)
  end) end)
  end



  def parse_pattern_guard() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(optional((Nova.Runtime.bind(parse_binder(), fn b ->
  Nova.Runtime.bind(tok_left_arrow(), fn arr ->
    Nova.Runtime.pure(({:tuple, b, arr}))
  end)
end))), fn pat_bind ->
    Nova.Runtime.bind(parse_expr(), fn expr ->
      Nova.Runtime.pure(%{binder: pat_bind, expr: expr})
    end)
  end) end)
  end



  def parse_expr_atom() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(parse_expr_ident(), parse_expr_constructor()), parse_expr_literal()), parse_expr_array()), parse_expr_record()), parse_expr_parens()), parse_expr_section()), parse_expr_hole()) end)
  end



  def parse_expr_section() do
      Nova.Runtime.bind(token(Nova.Compiler.Cst.tok_underscore()), fn tok ->
    Nova.Runtime.pure((Nova.Compiler.Cst.expr_section(tok)))
  end)
  end



  def parse_expr_hole() do
      Nova.Runtime.bind(tok_hole(), fn {:tuple, tok, hole_name} ->
        full_name = Nova.Runtime.append("_", hole_name)
        name = %{token: tok, name: Nova.Compiler.Cst.ident(full_name)}
    Nova.Runtime.pure((Nova.Compiler.Cst.expr_hole(name)))
  end)
  end



  def parse_expr_ident() do
    Nova.Runtime.fmap((&Nova.Compiler.Cst.expr_ident/1), tok_qualified_lower_name())
  end



  def parse_expr_constructor() do
    Nova.Runtime.fmap((&Nova.Compiler.Cst.expr_constructor/1), tok_qualified_upper_name())
  end



  def parse_expr_literal() do
    Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(parse_expr_string(), parse_expr_int()), parse_expr_number()), parse_expr_char()), parse_expr_bool())
  end



  def parse_expr_string() do
      Nova.Runtime.bind(tok_string(), fn {:tuple, tok, s} ->
    Nova.Runtime.pure((Nova.Compiler.Cst.expr_string(tok, s)))
  end)
  end



  def parse_expr_int() do
      Nova.Runtime.bind(tok_int(), fn {:tuple, tok, n} ->
    Nova.Runtime.pure((Nova.Compiler.Cst.expr_int(tok, n)))
  end)
  end



  def parse_expr_number() do
      Nova.Runtime.bind(tok_number(), fn {:tuple, tok, n} ->
    Nova.Runtime.pure((Nova.Compiler.Cst.expr_number(tok, n)))
  end)
  end



  def parse_expr_char() do
      Nova.Runtime.bind(tok_char(), fn {:tuple, tok, c} ->
    Nova.Runtime.pure((Nova.Compiler.Cst.expr_char(tok, c)))
  end)
  end



  def parse_expr_bool() do
    
      parse_true =    Nova.Runtime.bind(tok_keyword("true"), fn tok ->
     Nova.Runtime.pure((Nova.Compiler.Cst.expr_boolean(tok, true)))
   end)
      parse_false =    Nova.Runtime.bind(tok_keyword("false"), fn tok ->
     Nova.Runtime.pure((Nova.Compiler.Cst.expr_boolean(tok, false)))
   end)
      Nova.Runtime.alt(parse_true, parse_false)
  end



  def parse_expr_array() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(delimited(tok_left_square(), parse_expr(), tok_comma(), tok_right_square()), fn d ->
    Nova.Runtime.pure((Nova.Compiler.Cst.expr_array(d)))
  end) end)
  end



  def parse_expr_record() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(delimited(tok_left_brace(), parse_record_field(), tok_comma(), tok_right_brace()), fn d ->
    Nova.Runtime.pure((Nova.Compiler.Cst.expr_record(d)))
  end) end)
  end



  def parse_record_field() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.alt(parse_record_field_full(), parse_record_pun()) end)
  end



  def parse_record_field_full() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_label(), fn label ->
    Nova.Runtime.bind(Nova.Runtime.alt(tok_colon(), tok_equals()), fn sep ->
      Nova.Runtime.bind(parse_expr(), fn value ->
        Nova.Runtime.pure((Nova.Compiler.Cst.record_field(label, sep, value)))
      end)
    end)
  end) end)
  end



  def parse_record_pun() do
    Nova.Runtime.fmap((&Nova.Compiler.Cst.record_pun/1), tok_lower_name())
  end



  def parse_expr_parens() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(wrapped(tok_left_paren(), parse_expr(), tok_right_paren()), fn w ->
    Nova.Runtime.pure((Nova.Compiler.Cst.expr_parens(w)))
  end) end)
  end



  def parse_binder() do
    Control.Lazy.defer(fn _ -> parse_binder1() end)
  end



  def parse_binder1() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_binder2(), fn b ->
    Nova.Runtime.bind(many((Nova.Runtime.bind(tok_operator(), fn op ->
  Nova.Runtime.bind(parse_binder2(), fn b2 ->
    Nova.Runtime.pure(({:tuple, op, b2}))
  end)
end))), fn ops ->
      if Nova.List.null(ops) do
  Nova.Runtime.pure(b)
else
  Nova.Runtime.pure((Nova.Compiler.Cst.binder_op(b, ops)))
end
    end)
  end) end)
  end



  def parse_binder2() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_binder3(), fn b ->
    Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_double_colon(), fn dc ->
  Nova.Runtime.bind(parse_type(), fn t ->
    Nova.Runtime.pure(({:tuple, dc, t}))
  end)
end))), fn ann ->
      case ann do
  {:just, ({:tuple, dc, t})} -> Nova.Runtime.pure((Nova.Compiler.Cst.binder_typed(b, dc, t)))
  :nothing -> Nova.Runtime.pure(b)
end
    end)
  end) end)
  end



  def parse_binder3() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.alt(parse_binder_con(), parse_binder_atom()) end)
  end



  def parse_binder_con() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_qualified_upper_name(), fn con ->
    Nova.Runtime.bind(some(parse_binder_atom()), fn args ->
      Nova.Runtime.pure((Nova.Compiler.Cst.binder_constructor(con, args)))
    end)
  end) end)
  end



  def parse_binder_atom() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(parse_binder_wildcard(), parse_binder_hole()), parse_binder_nullary_con()), parse_binder_var()), parse_binder_literal()), parse_binder_array()), parse_binder_record()), parse_binder_parens()) end)
  end



  def parse_binder_nullary_con() do
      Nova.Runtime.bind(tok_qualified_upper_name(), fn con ->
    Nova.Runtime.pure((Nova.Compiler.Cst.binder_constructor(con, [])))
  end)
  end



  def parse_binder_wildcard() do
      Nova.Runtime.bind(token(Nova.Compiler.Cst.tok_underscore()), fn tok ->
    Nova.Runtime.pure((Nova.Compiler.Cst.binder_wildcard(tok)))
  end)
  end



  def parse_binder_hole() do
      Nova.Runtime.bind(tok_hole(), fn {:tuple, tok, hole_name} ->
        full_name = Nova.Runtime.append("_", hole_name)
        name = %{token: tok, name: Nova.Compiler.Cst.ident(full_name)}
    Nova.Runtime.pure((Nova.Compiler.Cst.binder_var(name)))
  end)
  end



  def parse_binder_var() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(tok_lower_name(), fn name ->
    Nova.Runtime.bind(optional((Nova.Runtime.bind(token(Nova.Compiler.Cst.tok_at()), fn at ->
  Nova.Runtime.bind(parse_binder_atom(), fn b ->
    Nova.Runtime.pure(({:tuple, at, b}))
  end)
end))), fn named ->
      case named do
  {:just, ({:tuple, at, b})} -> Nova.Runtime.pure((Nova.Compiler.Cst.binder_named(name, at, b)))
  :nothing -> Nova.Runtime.pure((Nova.Compiler.Cst.binder_var(name)))
end
    end)
  end) end)
  end



  def parse_binder_literal() do
    Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(parse_binder_string(), parse_binder_int()), parse_binder_number()), parse_binder_char()), parse_binder_bool())
  end



  def parse_binder_string() do
      Nova.Runtime.bind(tok_string(), fn {:tuple, tok, s} ->
    Nova.Runtime.pure((Nova.Compiler.Cst.binder_string(tok, s)))
  end)
  end



  def parse_binder_int() do
    
      is_neg = fn auto_arg0 -> case auto_arg0 do
        ({:tok_operator, :nothing, "-"}) -> true
        _ -> false
      end end
      Nova.Runtime.bind(optional((satisfy(is_neg))), fn neg ->
  Nova.Runtime.bind(tok_int(), fn {:tuple, tok, n} ->
    Nova.Runtime.pure((Nova.Compiler.Cst.binder_int(neg, tok, n)))
  end)
end)
  end



  def parse_binder_number() do
    
      is_neg = fn auto_arg0 -> case auto_arg0 do
        ({:tok_operator, :nothing, "-"}) -> true
        _ -> false
      end end
      Nova.Runtime.bind(optional((satisfy(is_neg))), fn neg ->
  Nova.Runtime.bind(tok_number(), fn {:tuple, tok, n} ->
    Nova.Runtime.pure((Nova.Compiler.Cst.binder_number(neg, tok, n)))
  end)
end)
  end



  def parse_binder_char() do
      Nova.Runtime.bind(tok_char(), fn {:tuple, tok, c} ->
    Nova.Runtime.pure((Nova.Compiler.Cst.binder_char(tok, c)))
  end)
  end



  def parse_binder_bool() do
    
      parse_binder_true =    Nova.Runtime.bind(tok_keyword("true"), fn tok ->
     Nova.Runtime.pure((Nova.Compiler.Cst.binder_boolean(tok, true)))
   end)
      parse_binder_false =    Nova.Runtime.bind(tok_keyword("false"), fn tok ->
     Nova.Runtime.pure((Nova.Compiler.Cst.binder_boolean(tok, false)))
   end)
      Nova.Runtime.alt(parse_binder_true, parse_binder_false)
  end



  def parse_binder_array() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(delimited(tok_left_square(), parse_binder(), tok_comma(), tok_right_square()), fn d ->
    Nova.Runtime.pure((Nova.Compiler.Cst.binder_array(d)))
  end) end)
  end



  def parse_binder_record() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(delimited(tok_left_brace(), parse_binder_record_field(), tok_comma(), tok_right_brace()), fn d ->
    Nova.Runtime.pure((Nova.Compiler.Cst.binder_record(d)))
  end) end)
  end



  def parse_binder_record_field() do
    Control.Lazy.defer(fn _ -> Nova.Runtime.alt(parse_binder_record_full(), parse_binder_record_pun()) end)
  end



  def parse_binder_record_full() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(parse_label(), fn label ->
    Nova.Runtime.bind(Nova.Runtime.alt(tok_colon(), tok_equals()), fn sep ->
      Nova.Runtime.bind(parse_binder(), fn value ->
        Nova.Runtime.pure((Nova.Compiler.Cst.record_field(label, sep, value)))
      end)
    end)
  end) end)
  end



  def parse_binder_record_pun() do
    Nova.Runtime.fmap((&Nova.Compiler.Cst.record_pun/1), tok_lower_name())
  end



  def parse_binder_parens() do
    Control.Lazy.defer(fn _ ->   Nova.Runtime.bind(wrapped(tok_left_paren(), parse_binder(), tok_right_paren()), fn w ->
    Nova.Runtime.pure((Nova.Compiler.Cst.binder_parens(w)))
  end) end)
  end



  def parse_module() do
      Nova.Runtime.bind(tok_keyword("module"), fn kw ->
    Nova.Runtime.bind(parse_module_name(), fn name ->
      Nova.Runtime.bind(optional(parse_exports()), fn exports ->
        Nova.Runtime.bind(tok_keyword("where"), fn where_kw ->
          Nova.Runtime.bind(tok_layout_start(), fn _ ->
            Nova.Runtime.bind(parse_module_imports(), fn imports ->
              Nova.Runtime.bind(parse_module_decls(), fn decls ->
                Nova.Runtime.bind(optional(tok_layout_end()), fn _ ->
                                    header = %{keyword: kw, name: name, exports: exports, where: where_kw, imports: imports}
                                    body = %{decls: decls, trailing_comments: [], end_: %{line: 0, column: 0}}
                  Nova.Runtime.pure(%{header: header, body: body})
                end)
              end)
            end)
          end)
        end)
      end)
    end)
  end)
  end



  def parse_module_imports() do
    
      go = Nova.Runtime.fix(fn go -> fn acc -> Nova.Runtime.alt((Nova.Runtime.bind(parse_import(), fn imp ->
  Nova.Runtime.bind(optional(tok_layout_sep()), fn sep ->
    case sep do
  {:just, _} -> go.((Nova.Runtime.append(acc, ([imp | []]))))
  :nothing -> Nova.Runtime.pure((Nova.Runtime.append(acc, ([imp | []]))))
end
  end)
end)), Nova.Runtime.pure(acc))  end end)
      go.([])
  end



  def parse_module_decls() do
    
      go = Nova.Runtime.fix(fn go -> fn acc -> Nova.Runtime.alt((Nova.Runtime.bind(parse_declaration(), fn decl ->
  Nova.Runtime.bind(optional(tok_layout_sep()), fn sep ->
    case sep do
  {:just, _} -> go.((Nova.Runtime.append(acc, ([decl | []]))))
  :nothing -> Nova.Runtime.pure((Nova.Runtime.append(acc, ([decl | []]))))
end
  end)
end)), Nova.Runtime.pure(acc))  end end)
      Nova.Runtime.bind(optional(tok_layout_sep()), fn _ ->
  go.([])
end)
  end



  def parse_module_header() do
      Nova.Runtime.bind(tok_keyword("module"), fn kw ->
    Nova.Runtime.bind(parse_module_name(), fn name ->
      Nova.Runtime.bind(optional(parse_exports()), fn exports ->
        Nova.Runtime.bind(tok_keyword("where"), fn where_kw ->
          Nova.Runtime.pure(%{keyword: kw, name: name, exports: exports, where: where_kw, imports: []})
        end)
      end)
    end)
  end)
  end



  def parse_module_name() do
    
      extract_module_name = fn tok -> case tok do
        {:tok_upper_name, ({:just, prefix}), proper} -> {:just, (Nova.Compiler.Cst.module_name((Nova.Runtime.append(Nova.Runtime.append(prefix, "."), proper))))}
        {:tok_upper_name, :nothing, proper} -> {:just, (Nova.Compiler.Cst.module_name(proper))}
        _ -> :nothing
      end end
      Nova.Runtime.bind(expect_map(extract_module_name), fn {:tuple, tok, name} ->
  Nova.Runtime.pure(%{token: tok, name: name})
end)
  end



  def parse_exports() do
      Nova.Runtime.bind(tok_left_paren(), fn open ->
    Nova.Runtime.bind(separated(parse_export(), tok_comma()), fn exports ->
      Nova.Runtime.bind(tok_right_paren(), fn close ->
        Nova.Runtime.pure(%{open: open, value: exports, close: close})
      end)
    end)
  end)
  end



  def parse_export() do
    Nova.Runtime.alt(Nova.Runtime.alt(parse_export_value(), parse_export_type()), parse_export_module())
  end



  def parse_export_value() do
    Nova.Runtime.fmap((&Nova.Compiler.Cst.export_value/1), tok_lower_name())
  end



  def parse_export_type() do
      Nova.Runtime.bind(tok_upper_name(), fn name ->
    Nova.Runtime.bind(optional(parse_data_members()), fn members ->
      Nova.Runtime.pure((Nova.Compiler.Cst.export_type(name, members)))
    end)
  end)
  end



  def parse_export_module() do
      Nova.Runtime.bind(tok_keyword("module"), fn kw ->
    Nova.Runtime.bind(parse_module_name(), fn name ->
      Nova.Runtime.pure((Nova.Compiler.Cst.export_module(kw, name)))
    end)
  end)
  end



  def parse_data_members() do
    Nova.Runtime.alt(parse_data_all(), parse_data_enumerated())
  end



  def parse_data_all() do
      Nova.Runtime.bind(tok_left_paren(), fn _ ->
    Nova.Runtime.bind(tok_double_dot(), fn dotdot ->
      Nova.Runtime.bind(tok_right_paren(), fn _ ->
        Nova.Runtime.pure((Nova.Compiler.Cst.data_all(dotdot)))
      end)
    end)
  end)
  end



  def parse_data_enumerated() do
      Nova.Runtime.bind(delimited(tok_left_paren(), tok_upper_name(), tok_comma(), tok_right_paren()), fn d ->
    Nova.Runtime.pure((Nova.Compiler.Cst.data_enumerated(d)))
  end)
  end



  def parse_import() do
      Nova.Runtime.bind(tok_keyword("import"), fn kw ->
    Nova.Runtime.bind(parse_module_name(), fn mod_ ->
      Nova.Runtime.bind(optional((Nova.Runtime.bind(optional((tok_keyword("hiding"))), fn is_hiding ->
  Nova.Runtime.bind(wrapped(tok_left_paren(), (separated(parse_import_item(), tok_comma())), tok_right_paren()), fn imports ->
    Nova.Runtime.pure(({:tuple, is_hiding, imports}))
  end)
end))), fn names ->
        Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_keyword("as"), fn as_kw ->
  Nova.Runtime.bind(parse_module_name(), fn alias_ ->
    Nova.Runtime.pure(({:tuple, as_kw, alias_}))
  end)
end))), fn qualified ->
          Nova.Runtime.pure(%{keyword: kw, module: mod_, names: names, qualified: qualified})
        end)
      end)
    end)
  end)
  end



  def parse_import_item() do
    Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(parse_import_op(), parse_import_value()), parse_import_type()), parse_import_class())
  end



  def parse_import_op() do
      Nova.Runtime.bind(tok_left_paren(), fn _ ->
    Nova.Runtime.bind(parse_op_name(), fn op ->
      Nova.Runtime.bind(tok_right_paren(), fn _ ->
        Nova.Runtime.pure((Nova.Compiler.Cst.import_op(op)))
      end)
    end)
  end)
  end



  def parse_import_value() do
    Nova.Runtime.fmap((&Nova.Compiler.Cst.import_value/1), tok_lower_name())
  end



  def parse_import_type() do
      Nova.Runtime.bind(tok_upper_name(), fn name ->
    Nova.Runtime.bind(optional(parse_data_members()), fn members ->
      Nova.Runtime.pure((Nova.Compiler.Cst.import_type(name, members)))
    end)
  end)
  end



  def parse_import_class() do
      Nova.Runtime.bind(tok_keyword("class"), fn kw ->
    Nova.Runtime.bind(tok_upper_name(), fn name ->
      Nova.Runtime.pure((Nova.Compiler.Cst.import_class(kw, name)))
    end)
  end)
  end



  def parse_module_body() do
      Nova.Runtime.bind(layout_block(parse_declaration()), fn decls ->
    Nova.Runtime.pure(%{decls: decls, trailing_comments: [], end_: %{line: 0, column: 0}})
  end)
  end



  def parse_declaration() do
    Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(Nova.Runtime.alt(parse_decl_data(), parse_decl_type()), parse_decl_newtype()), parse_decl_class()), parse_decl_derive()), parse_decl_instance()), parse_decl_foreign()), parse_decl_fixity()), parse_decl_signature()), parse_decl_value())
  end



  def parse_decl_data() do
      Nova.Runtime.bind(tok_keyword("data"), fn kw ->
    Nova.Runtime.bind(tok_upper_name(), fn name ->
      Nova.Runtime.bind(many(parse_type_var_binding()), fn vars ->
        Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_equals(), fn eq ->
  Nova.Runtime.bind(separated(parse_data_ctor(), tok_pipe()), fn cs ->
    Nova.Runtime.pure(({:tuple, eq, cs}))
  end)
end))), fn ctors ->
          Nova.Runtime.pure((Nova.Compiler.Cst.decl_data(%{keyword: kw, name: name, vars: vars}, ctors)))
        end)
      end)
    end)
  end)
  end



  def parse_data_ctor() do
      Nova.Runtime.bind(tok_upper_name(), fn name ->
    Nova.Runtime.bind(many(parse_type_atom()), fn fields ->
      Nova.Runtime.pure(%{name: name, fields: fields})
    end)
  end)
  end



  def parse_type_var_binding() do
    parse_type_var_name()
  end



  def parse_type_var_name() do
    Nova.Runtime.fmap((&Nova.Compiler.Cst.type_var_name/1), tok_lower_name())
  end



  def parse_decl_type() do
      Nova.Runtime.bind(tok_keyword("type"), fn kw ->
    Nova.Runtime.bind(tok_upper_name(), fn name ->
      Nova.Runtime.bind(many(parse_type_var_binding()), fn vars ->
        Nova.Runtime.bind(tok_equals(), fn eq ->
          Nova.Runtime.bind(parse_type(), fn ty ->
            Nova.Runtime.pure((Nova.Compiler.Cst.decl_type(%{keyword: kw, name: name, vars: vars}, eq, ty)))
          end)
        end)
      end)
    end)
  end)
  end



  def parse_decl_newtype() do
      Nova.Runtime.bind(tok_keyword("newtype"), fn kw ->
    Nova.Runtime.bind(tok_upper_name(), fn name ->
      Nova.Runtime.bind(many(parse_type_var_binding()), fn vars ->
        Nova.Runtime.bind(tok_equals(), fn eq ->
          Nova.Runtime.bind(tok_upper_name(), fn ctor_name ->
            Nova.Runtime.bind(parse_type_atom(), fn wrapped_ty ->
              Nova.Runtime.pure((Nova.Compiler.Cst.decl_newtype(%{keyword: kw, name: name, vars: vars}, eq, ctor_name, wrapped_ty)))
            end)
          end)
        end)
      end)
    end)
  end)
  end



  def parse_decl_class() do
      Nova.Runtime.bind(tok_keyword("class"), fn kw ->
    Nova.Runtime.bind(tok_upper_name(), fn name ->
      Nova.Runtime.bind(many(parse_type_var_binding()), fn vars ->
        Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_keyword("where"), fn where_kw ->
  Nova.Runtime.bind(layout_block(parse_class_member()), fn ms ->
    Nova.Runtime.pure(({:tuple, where_kw, ms}))
  end)
end))), fn methods ->
          Nova.Runtime.pure((Nova.Compiler.Cst.decl_class(%{keyword: kw, super_: :nothing, name: name, vars: vars, fundeps: :nothing}, methods)))
        end)
      end)
    end)
  end)
  end



  def parse_class_member() do
      Nova.Runtime.bind(tok_lower_name(), fn name ->
    Nova.Runtime.bind(tok_double_colon(), fn dc ->
      Nova.Runtime.bind(parse_type(), fn ty ->
        Nova.Runtime.pure(%{label: name, separator: dc, value: ty})
      end)
    end)
  end)
  end



  def parse_decl_instance() do
      Nova.Runtime.bind(tok_keyword("instance"), fn kw ->
    Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_lower_name(), fn n ->
  Nova.Runtime.bind(tok_double_colon(), fn dc ->
    Nova.Runtime.pure(({:tuple, n, dc}))
  end)
end))), fn name ->
      Nova.Runtime.bind(tok_qualified_upper_name(), fn class_name ->
        Nova.Runtime.bind(many(parse_type_atom()), fn types ->
          Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_keyword("where"), fn where_kw ->
  Nova.Runtime.bind(layout_block(parse_instance_binding()), fn bindings ->
    Nova.Runtime.pure(({:tuple, where_kw, bindings}))
  end)
end))), fn body ->
                        instance_head = %{keyword: kw, name: name, constraints: :nothing, class_name: class_name, types: types}
                        inst = %{head: instance_head, body: body}
            Nova.Runtime.pure((Nova.Compiler.Cst.decl_instance_chain(%{head: inst, tail: []})))
          end)
        end)
      end)
    end)
  end)
  end



  def parse_instance_binding() do
    Nova.Runtime.alt(parse_instance_sig(), parse_instance_name())
  end



  def parse_instance_sig() do
      Nova.Runtime.bind(tok_lower_name(), fn name ->
    Nova.Runtime.bind(tok_double_colon(), fn dc ->
      Nova.Runtime.bind(parse_type(), fn ty ->
        Nova.Runtime.pure((Nova.Compiler.Cst.instance_binding_signature(%{label: name, separator: dc, value: ty})))
      end)
    end)
  end)
  end



  def parse_instance_name() do
      Nova.Runtime.bind(tok_lower_name(), fn name ->
    Nova.Runtime.bind(many(parse_binder()), fn binders ->
      Nova.Runtime.bind(parse_guarded(), fn guarded ->
        Nova.Runtime.pure((Nova.Compiler.Cst.instance_binding_name(%{name: name, binders: binders, guarded: guarded})))
      end)
    end)
  end)
  end



  def parse_decl_derive() do
      Nova.Runtime.bind(tok_keyword("derive"), fn derive_kw ->
    Nova.Runtime.bind(optional((tok_keyword("newtype"))), fn newtype_kw ->
      Nova.Runtime.bind(tok_keyword("instance"), fn instance_kw ->
        Nova.Runtime.bind(optional((Nova.Runtime.bind(tok_lower_name(), fn n ->
  Nova.Runtime.bind(tok_double_colon(), fn dc ->
    Nova.Runtime.pure(({:tuple, n, dc}))
  end)
end))), fn name ->
          Nova.Runtime.bind(tok_qualified_upper_name(), fn class_name ->
            Nova.Runtime.bind(many(parse_type_atom()), fn types ->
                            instance_head = %{keyword: instance_kw, name: name, constraints: :nothing, class_name: class_name, types: types}
              Nova.Runtime.pure((Nova.Compiler.Cst.decl_derive(derive_kw, newtype_kw, instance_head)))
            end)
          end)
        end)
      end)
    end)
  end)
  end



  def parse_decl_foreign() do
      Nova.Runtime.bind(tok_keyword("foreign"), fn foreign_ ->
    Nova.Runtime.bind(tok_keyword("import"), fn import_ ->
      Nova.Runtime.bind(parse_foreign_value(), fn foreign_prime ->
        Nova.Runtime.pure((Nova.Compiler.Cst.decl_foreign(foreign_, import_, foreign_prime)))
      end)
    end)
  end)
  end



  def parse_foreign_value() do
      Nova.Runtime.bind(tok_lower_name(), fn name ->
    Nova.Runtime.bind(tok_double_colon(), fn dc ->
      Nova.Runtime.bind(parse_type(), fn ty ->
        Nova.Runtime.pure((Nova.Compiler.Cst.foreign_value(%{label: name, separator: dc, value: ty})))
      end)
    end)
  end)
  end



  def parse_decl_fixity() do
    
      int_value_to_int = fn auto_arg0 -> case auto_arg0 do
        ({:small_int, n}) -> n
        ({:big_int, _}) -> 0
      end end
      Nova.Runtime.bind(parse_fixity_keyword(), fn {:tuple, kw, fixity} ->
  Nova.Runtime.bind(tok_int(), fn {:tuple, prec_tok, prec} ->
    Nova.Runtime.bind(parse_fixity_op(), fn op ->
      Nova.Runtime.pure((Nova.Compiler.Cst.decl_fixity(%{keyword: {:tuple, kw, fixity}, prec: {:tuple, prec_tok, (int_value_to_int.(prec))}, operator: op})))
    end)
  end)
end)
  end



  def parse_fixity_keyword() do
    
      parse_infix =    Nova.Runtime.bind(tok_keyword("infix"), fn tok ->
     Nova.Runtime.pure(({:tuple, tok, Nova.Compiler.Cst.infix()}))
   end)
      parse_infixl =    Nova.Runtime.bind(tok_keyword("infixl"), fn tok ->
     Nova.Runtime.pure(({:tuple, tok, Nova.Compiler.Cst.infixl()}))
   end)
      parse_infixr =    Nova.Runtime.bind(tok_keyword("infixr"), fn tok ->
     Nova.Runtime.pure(({:tuple, tok, Nova.Compiler.Cst.infixr()}))
   end)
      Nova.Runtime.alt(Nova.Runtime.alt(parse_infix, parse_infixl), parse_infixr)
  end



  def parse_fixity_op() do
    
      to_either_name = fn qn -> %{token: qn.token, module: qn.module, name: {:left, qn.name}} end
      Nova.Runtime.bind(tok_qualified_lower_name(), fn name ->
  Nova.Runtime.bind(tok_keyword("as"), fn as_kw ->
    Nova.Runtime.bind(parse_op_name(), fn op ->
      Nova.Runtime.pure((Nova.Compiler.Cst.fixity_value((to_either_name.(name)), as_kw, op)))
    end)
  end)
end)
  end



  def parse_op_name() do
      Nova.Runtime.bind(tok_operator(), fn op ->
    Nova.Runtime.pure(%{token: op.token, name: op.name})
  end)
  end



  def parse_decl_signature() do
      Nova.Runtime.bind(tok_lower_name(), fn name ->
    Nova.Runtime.bind(tok_double_colon(), fn dc ->
      Nova.Runtime.bind(parse_type(), fn ty ->
        Nova.Runtime.pure((Nova.Compiler.Cst.decl_signature(%{label: name, separator: dc, value: ty})))
      end)
    end)
  end)
  end



  def parse_decl_value() do
      Nova.Runtime.bind(tok_lower_name(), fn name ->
    Nova.Runtime.bind(many(parse_binder_atom()), fn binders ->
      Nova.Runtime.bind(parse_guarded(), fn guarded ->
        Nova.Runtime.pure((Nova.Compiler.Cst.decl_value(%{name: name, binders: binders, guarded: guarded})))
      end)
    end)
  end)
  end
end
