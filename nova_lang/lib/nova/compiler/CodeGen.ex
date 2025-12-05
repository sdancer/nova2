defmodule Nova.Compiler.CodeGen do
  # import Prelude

  # import Data.Array

  # import Data.Array

  # import Data.List

  # import Data.List

  # import Data.Maybe

  # import Data.Tuple

  # import Data.String

  # import Data.String.CodeUnits

  # import Data.Set

  # import Data.Set

  # import Data.Foldable

  # import Nova.Compiler.Ast

  # @type gen_ctx :: %{module_funcs: set()(string()), locals: set()(string()), func_arities: array()(%{name: string(), arity: int()}), local_arities: array()(%{name: string(), arity: int()})}



  def empty_ctx() do
    %{module_funcs: Nova.Set.empty, locals: Nova.Set.empty, func_arities: [], local_arities: []}
  end



  def lookup_local_arity(name, ctx) do
    case Nova.Array.find((fn x -> (x.name == name) end), ctx.local_arities) do
      {:just, rec} -> {:just, rec.arity}
      :nothing -> :nothing
    end
  end



  def add_local_arity(name, arity, ctx) do
    %{ctx | local_arities: [%{name: name, arity: arity} | ctx.local_arities]}
  end



  def add_locals_from_pattern(({:pat_var, name}), ctx) do
    %{ctx | locals: Nova.Set.insert(name, ctx.locals)}
  end

  def add_locals_from_pattern(:pat_wildcard, ctx) do
    ctx
  end

  def add_locals_from_pattern(({:pat_lit, _}), ctx) do
    ctx
  end

  def add_locals_from_pattern(({:pat_con, _, pats}), ctx) do
    Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, pats)
  end

  def add_locals_from_pattern(({:pat_record, fields}), ctx) do
    Nova.Runtime.foldr((fn ({:tuple, _, p}) -> fn c -> add_locals_from_pattern(p, c) end end), ctx, fields)
  end

  def add_locals_from_pattern(({:pat_list, pats}), ctx) do
    Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, pats)
  end

  def add_locals_from_pattern(({:pat_cons, hd, tl}), ctx) do
    add_locals_from_pattern(tl, (add_locals_from_pattern(hd, ctx)))
  end

  def add_locals_from_pattern(({:pat_as, name, pat}), ctx) do
    add_locals_from_pattern(pat, (%{ctx | locals: Nova.Set.insert(name, ctx.locals)}))
  end

  def add_locals_from_pattern(({:pat_parens, p}), ctx) do
    add_locals_from_pattern(p, ctx)
  end



  def free_vars_expr(({:expr_var, name})) do
    Nova.Set.singleton(name)
  end

  def free_vars_expr(({:expr_lit, _})) do
    Nova.Set.empty
  end

  def free_vars_expr(({:expr_app, f, arg})) do
    Nova.Set.union((free_vars_expr(f)), (free_vars_expr(arg)))
  end

  def free_vars_expr(({:expr_lambda, pats, body})) do
    
      bound = Nova.Runtime.foldr((fn p -> fn s -> Nova.Set.union((pattern_vars(p)), s) end end), Nova.Set.empty, pats)
      Nova.Set.difference((free_vars_expr(body)), bound)
  end

  def free_vars_expr(({:expr_let, binds, body})) do
    
      bound = Nova.Runtime.foldr((fn b -> fn s -> Nova.Set.union((pattern_vars(b.pattern)), s) end end), Nova.Set.empty, binds)
      bind_vars = Nova.Runtime.foldr((fn b -> fn s -> Nova.Set.union((free_vars_expr(b.value)), s) end end), Nova.Set.empty, binds)
      Nova.Set.union((Nova.Set.difference(bind_vars, bound)), (Nova.Set.difference((free_vars_expr(body)), bound)))
  end

  def free_vars_expr(({:expr_if, c, t, e})) do
    Nova.Set.union((free_vars_expr(c)), (Nova.Set.union((free_vars_expr(t)), (free_vars_expr(e)))))
  end

  def free_vars_expr(({:expr_case, scrut, clauses})) do
    
      scrut_vars = free_vars_expr(scrut)
      clause_vars = Nova.Runtime.foldr((fn cl -> fn s -> Nova.Set.union((free_vars_clause(cl)), s) end end), Nova.Set.empty, clauses)
      Nova.Set.union(scrut_vars, clause_vars)
  end

  def free_vars_expr(({:expr_bin_op, _, l, r})) do
    Nova.Set.union((free_vars_expr(l)), (free_vars_expr(r)))
  end

  def free_vars_expr(({:expr_unary_op, _, e})) do
    free_vars_expr(e)
  end

  def free_vars_expr(({:expr_list, elems})) do
    Nova.Runtime.foldr((fn e -> fn s -> Nova.Set.union((free_vars_expr(e)), s) end end), Nova.Set.empty, elems)
  end

  def free_vars_expr(({:expr_tuple, elems})) do
    Nova.Runtime.foldr((fn e -> fn s -> Nova.Set.union((free_vars_expr(e)), s) end end), Nova.Set.empty, elems)
  end

  def free_vars_expr(({:expr_record, fields})) do
    Nova.Runtime.foldr((fn ({:tuple, _, e}) -> fn s -> Nova.Set.union((free_vars_expr(e)), s) end end), Nova.Set.empty, fields)
  end

  def free_vars_expr(({:expr_record_access, e, _})) do
    free_vars_expr(e)
  end

  def free_vars_expr(({:expr_record_update, e, fields})) do
    Nova.Runtime.foldr((fn ({:tuple, _, v}) -> fn s -> Nova.Set.union((free_vars_expr(v)), s) end end), (free_vars_expr(e)), fields)
  end

  def free_vars_expr(({:expr_parens, e})) do
    free_vars_expr(e)
  end

  def free_vars_expr(({:expr_do, stmts})) do
    free_vars_do(stmts, Nova.Set.empty)
  end

  def free_vars_expr(({:expr_qualified, _, _})) do
    Nova.Set.empty
  end

  def free_vars_expr(({:expr_typed, e, _})) do
    free_vars_expr(e)
  end

  def free_vars_expr(({:expr_section, _})) do
    Nova.Set.empty
  end

  def free_vars_expr(({:expr_section_left, e, _})) do
    free_vars_expr(e)
  end

  def free_vars_expr(({:expr_section_right, _, e})) do
    free_vars_expr(e)
  end



  def free_vars_clause(cl) do
    
      bound = pattern_vars(cl.pattern)
      guard_vars = case cl.guard do
        :nothing -> Nova.Set.empty
        {:just, g} -> free_vars_expr(g)
      end
      body_vars = free_vars_expr(cl.body)
      Nova.Set.difference((Nova.Set.union(guard_vars, body_vars)), bound)
  end



  def used_vars_in_clause(cl) do
    
      guard_vars = case cl.guard do
        :nothing -> Nova.Set.empty
        {:just, g} -> free_vars_expr(g)
      end
      body_vars = free_vars_expr(cl.body)
      Nova.Set.union(guard_vars, body_vars)
  end



  def free_vars_do(stmts, bound) do
    case stmts do
      [] -> Nova.Set.empty
      [({:do_expr, e}) | rest] -> Nova.Set.union((Nova.Set.difference((free_vars_expr(e)), bound)), (free_vars_do(rest, bound)))
      [({:do_bind, pat, e}) | rest] -> 
          expr_vars = Nova.Set.difference((free_vars_expr(e)), bound)
          new_bound = Nova.Set.union(bound, (pattern_vars(pat)))
          Nova.Set.union(expr_vars, (free_vars_do(rest, new_bound)))
      [({:do_let, binds}) | rest] -> 
          bind_pat_vars = Nova.Runtime.foldr((fn b -> fn s -> Nova.Set.union((pattern_vars(b.pattern)), s) end end), Nova.Set.empty, binds)
          bind_vars = Nova.Runtime.foldr((fn b -> fn s -> Nova.Set.union((Nova.Set.difference((free_vars_expr(b.value)), bound)), s) end end), Nova.Set.empty, binds)
          new_bound = Nova.Set.union(bound, bind_pat_vars)
          Nova.Set.union(bind_vars, (free_vars_do(rest, new_bound)))
    end
  end



  def pattern_vars(({:pat_var, name})) do
    Nova.Set.singleton(name)
  end

  def pattern_vars(:pat_wildcard) do
    Nova.Set.empty
  end

  def pattern_vars(({:pat_lit, _})) do
    Nova.Set.empty
  end

  def pattern_vars(({:pat_con, _, pats})) do
    Nova.Runtime.foldr((fn p -> fn s -> Nova.Set.union((pattern_vars(p)), s) end end), Nova.Set.empty, pats)
  end

  def pattern_vars(({:pat_record, fields})) do
    Nova.Runtime.foldr((fn ({:tuple, _, p}) -> fn s -> Nova.Set.union((pattern_vars(p)), s) end end), Nova.Set.empty, fields)
  end

  def pattern_vars(({:pat_list, pats})) do
    Nova.Runtime.foldr((fn p -> fn s -> Nova.Set.union((pattern_vars(p)), s) end end), Nova.Set.empty, pats)
  end

  def pattern_vars(({:pat_cons, hd, tl})) do
    Nova.Set.union((pattern_vars(hd)), (pattern_vars(tl)))
  end

  def pattern_vars(({:pat_as, name, pat})) do
    Nova.Set.insert(name, (pattern_vars(pat)))
  end

  def pattern_vars(({:pat_parens, p})) do
    pattern_vars(p)
  end



  def collect_module_funcs(decls) do
    
      go = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {({:decl_function, func}), acc} -> Nova.Set.insert(func.name, acc)
        {({:decl_data_type, dt}), acc} -> Nova.Runtime.foldr((fn con -> fn s -> Nova.Set.insert(con.name, s) end end), acc, dt.constructors)
        {_, acc} -> acc
      end end end
      Nova.Runtime.foldr(go, Nova.Set.empty, decls)
  end



  def collect_func_arities(decls) do
    
      go = fn auto_arg0 -> case auto_arg0 do
        ({:decl_function, func}) -> {:just, %{name: func.name, arity: Nova.List.length(func.parameters)}}
        _ -> :nothing
      end end
      Nova.Array.map_maybe(go, decls)
  end



  def lookup_arity(name, ctx) do
    Nova.Runtime.map(& &1.arity, (Nova.Array.find((fn f -> (f.name == name) end), ctx.func_arities)))
  end



  def gen_module(mod_) do
    
      decls = Nova.Array.from_foldable(mod_.declarations)
      ctx = %{module_funcs: collect_module_funcs(decls), func_arities: collect_func_arities(decls), locals: Nova.Set.empty, local_arities: []}
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("defmodule ", elixir_module_name(mod_.name)), " do\n"), Nova.Runtime.intercalate("\n\n", (Nova.Runtime.map((fn auto_p0 -> gen_declaration(ctx, auto_p0) end), decls)))), "\nend\n")
  end



  def elixir_module_name(name) do
    Nova.String.replace_all((Nova.String.pattern(".")), (Nova.String.replacement(".")), name)
  end



  def gen_declaration(ctx, ({:decl_function, func})) do
    gen_function(ctx, func)
  end

  def gen_declaration(_, ({:decl_data_type, dt})) do
    gen_data_type(dt)
  end

  def gen_declaration(_, ({:decl_newtype, nt})) do
    gen_newtype(nt)
  end

  def gen_declaration(_, ({:decl_type_alias, ta})) do
    gen_type_alias(ta)
  end

  def gen_declaration(_, ({:decl_import, imp})) do
    Nova.Runtime.append("  # import ", imp.module_name)
  end

  def gen_declaration(_, ({:decl_type_sig, _})) do
    ""
  end

  def gen_declaration(_, ({:decl_infix, inf})) do
    gen_infix(inf)
  end

  def gen_declaration(_, ({:decl_foreign_import, fi})) do
    gen_foreign_import(fi)
  end

  def gen_declaration(_, ({:decl_type_class, tc})) do
    gen_type_class(tc)
  end

  def gen_declaration(_, ({:decl_type_class_instance, inst})) do
    gen_type_class_instance(inst)
  end

  def gen_declaration(_, _) do
    "  # unsupported declaration"
  end



  def gen_foreign_import(fi) do
    
      func_name = snake_case(fi.function_name)
      ffi_module = case fi.module_name do
        "" -> "Nova.FFI"
        m -> Nova.Runtime.append("Nova.FFI.", m)
      end
      alias_name = case fi.alias_ do
        {:just, a} -> snake_case(a)
        :nothing -> func_name
      end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", alias_name), "(arg), do: "), ffi_module), "."), func_name), "(arg)")
  end



  def gen_function(ctx, func) do
    case handle_point_free_alias(ctx, func) do
      {:just, code} -> code
      :nothing -> 
          ctx_with_params = Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, func.parameters)
          params = Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((&gen_pattern/1), func.parameters)))))
          if Nova.List.null(func.guards) do
  
    body = gen_expr_ctx(ctx_with_params, 2, func.body)
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case(func.name)), "("), params), ") do\n"), body), "\n"), "  end")
else
  
    body_code = gen_guarded_function_body(ctx_with_params, 2, (Nova.Array.from_foldable(func.guards)))
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case(func.name)), "("), params), ") do\n"), body_code), "\n"), "  end")
end
    end
  end



  def handle_point_free_alias(ctx, func) do
    if not((Nova.List.null(func.parameters))) do
      :nothing
    else
      case func.body do
        {:expr_var, ref_name} -> case lookup_arity(ref_name, ctx) do
            {:just, arity} when (arity > 0) -> 
                arg_names = Nova.Runtime.map((fn i -> Nova.Runtime.append("auto_arg", Nova.Runtime.show(i)) end), (Nova.Array.range(0, ((arity - 1)))))
                args_str = Nova.Runtime.intercalate(", ", arg_names)
                {:just, (Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case(func.name)), "("), args_str), ") do\n    "), snake_case(ref_name)), "("), args_str), ")\n  end"))}
            _ -> :nothing
          end
        _ -> case func.body do
            {:expr_app, ({:expr_qualified, mod_, fn_}), arg} -> if is_partial_app_of_arity2(func.body) do
                
                  arg_code = gen_expr_ctx(ctx, 0, arg)
                  func_name = translate_qualified(mod_, fn_)
                  {:just, (Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case(func.name)), "(auto_arg0) do\n    "), func_name), "("), arg_code), ", auto_arg0)\n  end"))}
              else
                :nothing
              end
            {:expr_app, ({:expr_var, fn_}), arg} -> if is_partial_app_of_arity2(func.body) do
                
                  arg_code = gen_expr_ctx(ctx, 0, arg)
                  {:just, (Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case(func.name)), "(auto_arg0) do\n    "), snake_case(fn_)), "("), arg_code), ", auto_arg0)\n  end"))}
              else
                :nothing
              end
            _ -> :nothing
          end
      end
    end
  end



  def is_partial_app_of_arity2(({:expr_app, ({:expr_qualified, "Array", fn_}), _})) do
    (((((((((fn_ == "filter") or (fn_ == "map")) or (fn_ == "find")) or (fn_ == "any")) or (fn_ == "all")) or (fn_ == "takeWhile")) or (fn_ == "dropWhile")) or (fn_ == "sortBy")) or (fn_ == "groupBy"))
  end

  def is_partial_app_of_arity2(({:expr_app, ({:expr_var, fn_}), _})) do
    (((((fn_ == "filter") or (fn_ == "map")) or (fn_ == "find")) or (fn_ == "any")) or (fn_ == "all"))
  end

  def is_partial_app_of_arity2(_) do
    false
  end



  def gen_guarded_function_body(ctx, indent, guards) do
    
      is_pat_guard = fn auto_arg0 -> case auto_arg0 do
        ({:guard_pat, _, _}) -> true
        _ -> false
      end end
      case Nova.Array.uncons(guards) do
  :nothing -> Nova.Runtime.append(ind(indent), "nil")
  {:just, %{head: first_guard, tail: rest_guards}} -> 
      pat_guards = Nova.Array.filter(is_pat_guard, (Nova.Array.from_foldable(first_guard.guards)))
      expr_guards = Nova.Array.filter((fn g -> not((is_pat_guard.(g))) end), (Nova.Array.from_foldable(first_guard.guards)))
      if Nova.Array.null(pat_guards) do
  
    guard_clauses = Nova.Runtime.map((fn auto_p0 -> gen_guarded_expr_for_cond(ctx, ((indent + 1)), auto_p0) end), guards)
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), "cond do\n"), Nova.Runtime.intercalate("\n", guard_clauses)), "\n"), ind(indent)), "end")
else
  gen_pattern_guard_case(ctx, indent, first_guard, rest_guards)
end
end
  end



  def gen_pattern_guard_case(ctx, indent, first_guard, rest_guards) do
    
      is_pat_guard = fn auto_arg0 -> case auto_arg0 do
        ({:guard_pat, _, _}) -> true
        _ -> false
      end end
      
  pat_guards = Nova.Array.filter(is_pat_guard, (Nova.Array.from_foldable(first_guard.guards)))
  expr_guards = Nova.Array.filter((fn g -> not((is_pat_guard.(g))) end), (Nova.Array.from_foldable(first_guard.guards)))
  case pat_guards do
  [{:guard_pat, pat, scrutinee_expr}] -> 
      scrutinee = gen_expr_ctx(ctx, 0, scrutinee_expr)
      pat_str = gen_pattern(pat)
      ctx_with_pat = add_locals_from_pattern(pat, ctx)
      fallthrough = if Nova.Array.null(rest_guards) do
        Nova.Runtime.append(ind(((indent + 2))), "nil")
      else
        gen_guarded_function_body(ctx, ((indent + 2)), rest_guards)
      end
      body_expr = gen_expr_ctx(ctx_with_pat, 0, first_guard.body)
      when_clause = if Nova.Array.null(expr_guards) do
        ""
      else
        Nova.Runtime.append(" when ", gen_guard_clauses_simple(ctx_with_pat, expr_guards))
      end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), "case "), scrutinee), " do\n"), ind(((indent + 1)))), pat_str), when_clause), " ->\n"), ind(((indent + 2)))), body_expr), "\n"), ind(((indent + 1)))), "_ ->\n"), fallthrough), "\n"), ind(indent)), "end")
  _ -> gen_nested_pattern_guards(ctx, indent, pat_guards, expr_guards, first_guard.body, rest_guards)
end
  end



  def gen_nested_pattern_guards(ctx, indent, pat_guards, expr_guards, body, rest_guards) do
    case Nova.Array.uncons(pat_guards) do
      :nothing -> if Nova.Array.null(expr_guards) do
          gen_expr_ctx(ctx, indent, body)
        else
          
            fallthrough = if Nova.Array.null(rest_guards) do
              Nova.Runtime.append(ind(((indent + 1))), "nil")
            else
              gen_guarded_function_body(ctx, ((indent + 1)), rest_guards)
            end
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), "if "), gen_guard_clauses_simple(ctx, expr_guards)), " do\n"), gen_expr_ctx(ctx, ((indent + 1)), body)), "\n"), ind(indent)), "else\n"), fallthrough), "\n"), ind(indent)), "end")
        end
      {:just, %{head: {:guard_pat, pat, scrutinee_expr}, tail: remaining_pat_guards}} -> 
          scrutinee = gen_expr_ctx(ctx, 0, scrutinee_expr)
          pat_str = gen_pattern(pat)
          ctx_with_pat = add_locals_from_pattern(pat, ctx)
          fallthrough = if Nova.Array.null(rest_guards) do
            Nova.Runtime.append(ind(((indent + 1))), "_ -> nil")
          else
            Nova.Runtime.append(Nova.Runtime.append(ind(((indent + 1))), "_ ->\n"), gen_guarded_function_body(ctx, ((indent + 2)), rest_guards))
          end
          inner_code = gen_nested_pattern_guards(ctx_with_pat, ((indent + 2)), remaining_pat_guards, expr_guards, body, rest_guards)
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), "case "), scrutinee), " do\n"), ind(((indent + 1)))), pat_str), " ->\n"), inner_code), "\n"), fallthrough), "\n"), ind(indent)), "end")
      {:just, %{head: {:guard_expr, _}, tail: _}} -> gen_guarded_function_body(ctx, indent, rest_guards)
    end
  end



  def gen_guard_clauses_simple(ctx, clauses) do
    
      extract_expr_guard = fn auto_arg0 -> case auto_arg0 do
        ({:guard_expr, e}) -> {:just, e}
        _ -> :nothing
      end end
      
  exprs = Nova.Array.map_maybe(extract_expr_guard, clauses)
  Nova.Runtime.intercalate(" and ", (Nova.Runtime.map((fn e -> gen_expr_ctx(ctx, 0, e) end), exprs)))
  end



  def gen_guarded_expr_for_cond(ctx, indent, ge) do
    
      expr_guards = Nova.Array.from_foldable(ge.guards)
      body_expr = gen_expr_ctx(ctx, 0, ge.body)
      guard_expr = gen_guard_clauses(ctx, expr_guards)
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), guard_expr), " ->\n"), ind(((indent + 1)))), body_expr)
  end



  def gen_guarded_expr(ctx, indent, ge) do
    
      is_pat_guard = fn auto_arg0 -> case auto_arg0 do
        ({:guard_pat, _, _}) -> true
        _ -> false
      end end
      
  pat_guards = Nova.Array.filter(is_pat_guard, (Nova.Array.from_foldable(ge.guards)))
  expr_guards = Nova.Array.filter((fn g -> not((is_pat_guard.(g))) end), (Nova.Array.from_foldable(ge.guards)))
  body_expr = gen_expr_ctx(ctx, 0, ge.body)
  indent_str = repeat_str(indent, " ")
  if Nova.Array.null(pat_guards) do
  
    guard_expr = gen_guard_clauses(ctx, expr_guards)
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(indent_str, guard_expr), " ->\n"), indent_str), "  "), body_expr)
else
  
    with_clauses = Nova.Runtime.map((fn auto_p0 -> gen_with_clause(ctx, auto_p0) end), pat_guards)
    expr_guard_code = if Nova.Array.null(expr_guards) do
      ""
    else
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(",\n", indent_str), "       true <- ("), gen_guard_clauses(ctx, expr_guards)), ")")
    end
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(indent_str, "true ->\n"), indent_str), "  with "), Nova.Runtime.intercalate((Nova.Runtime.append(Nova.Runtime.append(",\n", indent_str), "       ")), with_clauses)), expr_guard_code), " do\n"), indent_str), "    "), body_expr), "\n"), indent_str), "  else\n"), indent_str), "    _ -> :__guard_failed__\n"), indent_str), "  end")
end
  end



  def gen_with_clause(ctx, ({:guard_pat, pat, expr})) do
    Nova.Runtime.append(Nova.Runtime.append(gen_pattern(pat), " <- "), gen_expr_ctx(ctx, 0, expr))
  end

  def gen_with_clause(ctx, ({:guard_expr, expr})) do
    Nova.Runtime.append(Nova.Runtime.append("true <- (", gen_expr_ctx(ctx, 0, expr)), ")")
  end



  def gen_guard_clauses(ctx, clauses) do
    if Nova.Array.null(clauses) do
      "true"
    else
      Nova.Runtime.intercalate(" and ", (Nova.Runtime.map((fn auto_p0 -> gen_guard_clause(ctx, auto_p0) end), clauses)))
    end
  end



  def gen_guard_clause(ctx, ({:guard_expr, expr})) do
    Nova.Runtime.append(Nova.Runtime.append("(", gen_expr_ctx(ctx, 0, expr)), ")")
  end

  def gen_guard_clause(ctx, ({:guard_pat, pat, expr})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("match?(", gen_pattern(pat)), ", "), gen_expr_ctx(ctx, 0, expr)), ")")
  end



  def repeat_str(n, s) do
    if (n <= 0) do
      ""
    else
      Nova.Runtime.append(s, repeat_str(((n - 1)), s))
    end
  end



  def gen_pattern(({:pat_var, name})) do
    snake_case(name)
  end

  def gen_pattern(:pat_wildcard) do
    "_"
  end

  def gen_pattern(({:pat_lit, lit})) do
    gen_literal(lit)
  end

  def gen_pattern(({:pat_con, name, pats})) do
    
      con_name = case Nova.String.last_index_of((Nova.String.pattern(".")), name) do
        {:just, i} -> Nova.String.drop(((i + 1)), name)
        :nothing -> name
      end
      atom_name = snake_case(con_name)
      case con_name do
  "Nil" -> "[]"
  "Cons" -> case Nova.List.from_foldable(pats) do
      [h | ([t | []])] -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("[", gen_pattern(h)), " | "), gen_pattern(t)), "]")
      _ -> Nova.Runtime.append(Nova.Runtime.append("{:cons, ", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((&gen_pattern/1), pats)))))), "}")
    end
  _ -> if Nova.List.null(pats) do
      Nova.Runtime.append(":", atom_name)
    else
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:", snake_case(con_name)), ", "), Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((&gen_pattern/1), pats)))))), "}")
    end
end
  end

  def gen_pattern(({:pat_record, fields})) do
    
      gen_field_pattern = fn ({:tuple, label, pat}) -> Nova.Runtime.append(Nova.Runtime.append(snake_case(label), ": "), gen_pattern(pat)) end
      Nova.Runtime.append(Nova.Runtime.append("%{", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map(gen_field_pattern, fields)))))), "}")
  end

  def gen_pattern(({:pat_list, pats})) do
    Nova.Runtime.append(Nova.Runtime.append("[", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((&gen_pattern/1), pats)))))), "]")
  end

  def gen_pattern(({:pat_cons, head, tail})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("[", gen_pattern(head)), " | "), gen_pattern(tail)), "]")
  end

  def gen_pattern(({:pat_as, name, pat})) do
    Nova.Runtime.append(Nova.Runtime.append(gen_pattern(pat), " = "), snake_case(name))
  end

  def gen_pattern(({:pat_parens, p})) do
    Nova.Runtime.append(Nova.Runtime.append("(", gen_pattern(p)), ")")
  end



  def gen_pattern_with_used(used, ({:pat_var, name})) do
    if Nova.Set.member(name, used) do
      snake_case(name)
    else
      if (Nova.String.take(1, name) == "_") do
        "_"
      else
        Nova.Runtime.append("_", snake_case(name))
      end
    end
  end

  def gen_pattern_with_used(_, :pat_wildcard) do
    "_"
  end

  def gen_pattern_with_used(_, ({:pat_lit, lit})) do
    gen_literal(lit)
  end

  def gen_pattern_with_used(used, ({:pat_con, name, pats})) do
    
      con_name = case Nova.String.last_index_of((Nova.String.pattern(".")), name) do
        {:just, i} -> Nova.String.drop(((i + 1)), name)
        :nothing -> name
      end
      atom_name = snake_case(con_name)
      case con_name do
  "Nil" -> "[]"
  "Cons" -> case Nova.List.from_foldable(pats) do
      [h | ([t | []])] -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("[", gen_pattern_with_used(used, h)), " | "), gen_pattern_with_used(used, t)), "]")
      _ -> Nova.Runtime.append(Nova.Runtime.append("{:cons, ", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> gen_pattern_with_used(used, auto_p0) end), pats)))))), "}")
    end
  _ -> if Nova.List.null(pats) do
      Nova.Runtime.append(":", atom_name)
    else
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:", snake_case(con_name)), ", "), Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> gen_pattern_with_used(used, auto_p0) end), pats)))))), "}")
    end
end
  end

  def gen_pattern_with_used(used, ({:pat_record, fields})) do
    
      gen_field_pattern = fn ({:tuple, label, pat}) -> Nova.Runtime.append(Nova.Runtime.append(snake_case(label), ": "), gen_pattern_with_used(used, pat)) end
      Nova.Runtime.append(Nova.Runtime.append("%{", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map(gen_field_pattern, fields)))))), "}")
  end

  def gen_pattern_with_used(used, ({:pat_list, pats})) do
    Nova.Runtime.append(Nova.Runtime.append("[", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> gen_pattern_with_used(used, auto_p0) end), pats)))))), "]")
  end

  def gen_pattern_with_used(used, ({:pat_cons, head, tail})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("[", gen_pattern_with_used(used, head)), " | "), gen_pattern_with_used(used, tail)), "]")
  end

  def gen_pattern_with_used(used, ({:pat_as, name, pat})) do
    
      prefix = if Nova.Set.member(name, used) do
        ""
      else
        "_"
      end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(gen_pattern_with_used(used, pat), " = "), prefix), snake_case(name))
  end

  def gen_pattern_with_used(used, ({:pat_parens, p})) do
    Nova.Runtime.append(Nova.Runtime.append("(", gen_pattern_with_used(used, p)), ")")
  end



  def gen_chained_app(func_name, args, ctx, indent) do
    case Nova.Array.uncons(args) do
      :nothing -> func_name
      {:just, %{head: first_arg, tail: rest_args}} -> 
          first_app = Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(func_name, ".("), gen_expr_prime(ctx, indent, first_arg)), ")")
          Nova.Runtime.foldl((fn acc -> fn arg -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(acc, ".("), gen_expr_prime(ctx, indent, arg)), ")") end end), first_app, rest_args)
    end
  end



  def collect_args(expr) do
    
      go = Nova.Runtime.fix2(fn go -> fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {({:expr_app, f, a}), acc} -> go.(f).(([a | acc]))
        {f, acc} -> %{func: f, args: acc}
      end end end end)
      go.(expr).([])
  end



  def is_module_func(ctx, name) do
    (Nova.Set.member(name, ctx.module_funcs) and not((Nova.Set.member(name, ctx.locals))))
  end



  def gen_expr() do
    fn auto_p0 -> fn auto_p1 -> gen_expr_ctx(empty_ctx(), auto_p0, auto_p1) end end
  end



  def gen_expr_ctx(ctx, indent, expr) do
    Nova.Runtime.append(ind(indent), gen_expr_prime(ctx, indent, expr))
  end



  def is_data_constructor(name) do
    Nova.Array.elem(name, ["Tuple", "Tuple2", "Tuple3", "Tuple4", "Tuple5", "Just", "Nothing", "Left", "Right", "Cons", "Nil", "TyVar", "TyCon", "TyRecord", "TokKeyword", "TokIdentifier", "TokNumber", "TokString", "TokChar", "TokOperator", "TokDelimiter", "TokNewline", "TokUnrecognized", "ExprVar", "ExprLit", "ExprApp", "ExprLambda", "ExprLet", "ExprIf", "ExprCase", "ExprBinOp", "ExprList", "ExprRecord", "ExprRecordAccess", "ExprParens", "ExprDo", "ExprQualified", "ExprRecordUpdate", "ExprTyped", "ExprUnaryOp", "ExprTuple", "ExprSection", "ExprSectionLeft", "ExprSectionRight", "ExprNegate", "PatVar", "PatWildcard", "PatLit", "PatCon", "PatRecord", "PatList", "PatCons", "PatAs", "PatParens", "PatTyped", "LitInt", "LitString", "LitChar", "LitBool", "LitNumber", "DeclFunction", "DeclTypeSig", "DeclDataType", "DeclTypeAlias", "DeclModule", "DeclImport", "DeclTypeClass", "DeclTypeClassInstance", "DeclInfix", "DeclForeignImport", "DeclType", "TyExprCon", "TyExprVar", "TyExprApp", "TyExprArrow", "TyExprRecord", "TyExprForAll", "TyExprTuple", "TyExprConstrained", "TyExprParens", "OccursCheck", "TypeMismatch", "ArityMismatch", "RecordFieldMismatch", "UnifyErr", "UnboundVariable", "NotImplemented", "DoLet", "DoBind", "DoExpr", "GuardExpr", "GuardPat", "Parser"])
  end



  def get_ast_constructor_arity(name) do
    if (((name == "PatWildcard") or (name == "ImportAll")) or (name == "ImportNone")) do
      0
    else
      if (name == "ExprIf") do
        3
      else
        if is_arity2_constructor(name) do
          2
        else
          1
        end
      end
    end
  end



  def is_arity2_constructor(name) do
    Nova.Array.elem(name, arity2_constructors())
  end



  def arity2_constructors() do
    ["ExprApp", "ExprCase", "ExprBinOp", "ExprRecordAccess", "ExprRecordUpdate", "ExprQualified", "ExprTyped", "ExprUnaryOp", "ExprLambda", "ExprLet", "PatCon", "PatCons", "PatAs", "TyExprApp", "TyExprArrow", "TyExprRecord", "TyExprForAll", "TyExprConstrained", "DoBind", "GuardPat", "ImportType"]
  end



  def is_ast_constructor(name) do
    Nova.Array.elem(name, ["ExprVar", "ExprLit", "ExprApp", "ExprLambda", "ExprLet", "ExprIf", "ExprCase", "ExprBinOp", "ExprList", "ExprRecord", "ExprRecordAccess", "ExprParens", "ExprDo", "ExprQualified", "ExprRecordUpdate", "ExprTyped", "ExprUnaryOp", "ExprTuple", "ExprSection", "ExprSectionLeft", "ExprSectionRight", "ExprNegate", "PatVar", "PatWildcard", "PatLit", "PatCon", "PatRecord", "PatList", "PatCons", "PatAs", "PatParens", "PatTyped", "LitInt", "LitString", "LitChar", "LitBool", "LitNumber", "DeclFunction", "DeclTypeSig", "DeclDataType", "DeclTypeAlias", "DeclModule", "DeclImport", "DeclTypeClass", "DeclTypeClassInstance", "DeclInfix", "DeclForeignImport", "DeclType", "TyExprCon", "TyExprVar", "TyExprApp", "TyExprArrow", "TyExprRecord", "TyExprForAll", "TyExprTuple", "TyExprConstrained", "TyExprParens", "DoLet", "DoBind", "DoExpr", "GuardExpr", "GuardPat", "TokKeyword", "TokIdentifier", "TokNumber", "TokString", "TokChar", "TokOperator", "TokDelimiter", "TokNewline", "TokUnrecognized"])
  end



  def is_prelude_func(name) do
    Nova.Array.elem(name, ["show", "map", "foldl", "foldr", "foldM", "filter", "intercalate", "identity", "const", "compose", "pure", "otherwise", "length", "zip", "tuple", "just", "nothing", "left", "right", "fromMaybe", "maybe", "either", "isJust", "isNothing", "fst", "snd"])
  end



  def is_nullary_constructor(name) do
    Nova.Array.elem(name, ["TokKeyword", "TokIdentifier", "TokNumber", "TokString", "TokChar", "TokOperator", "TokDelimiter", "TokNewline", "TokUnrecognized", "PatWildcard", "ImportAll", "ImportNone", "KindFunction", "KindDataType", "KindTypeAlias", "KindTypeClass", "KindInstance", "KindForeignImport", "AssocLeft", "AssocRight", "AssocNone", "LytRoot", "LytTopDecl", "LytTopDeclHead", "LytDeclGuard", "LytCase", "LytCaseBinders", "LytCaseGuard", "LytLambdaBinders", "LytParen", "LytBrace", "LytSquare", "LytIf", "LytThen", "LytProperty", "LytForall", "LytTick", "LytLet", "LytLetStmt", "LytWhere", "LytOf", "LytDo", "LytAdo"])
  end



  def is_binary_func(name) do
    Nova.Array.elem(name, ["applySubst", "lookupSubst", "singleSubst", "composeSubst", "extendEnv", "lookupEnv", "unify", "bindVar"])
  end



  def is_types_module_func(name) do
    Nova.Array.elem(name, ["emptySubst", "singleSubst", "composeSubst", "applySubst", "freeTypeVars", "freeTypeVarsScheme", "freeTypeVarsEnv", "lookupSubst", "extendEnv", "lookupEnv", "applySubstToEnv", "freshVar", "generalize", "instantiate", "mkScheme", "mkTVar", "mkTCon", "mkTCon0", "tyVar", "tyCon", "tyRecord", "tInt", "tString", "tBool", "tChar", "tArray", "tArrow", "tMaybe", "tEither", "tTuple", "tMap", "tSet", "tList", "tNumber", "emptyEnv", "builtinPrelude", "emptyExports", "mergeTypeExport", "mergeExportsToEnv", "registerModule", "lookupModule"])
  end



  def is_types_module_value(name) do
    Nova.Array.elem(name, ["emptySubst", "emptyEnv", "emptyExports", "tInt", "tString", "tBool", "tChar", "tNumber"])
  end



  def types_module_func_arity(name) do
    if is_types_module_value(name) do
      0
    else
      if Nova.Array.elem(name, types_arity1_funcs()) do
        1
      else
        if Nova.Array.elem(name, types_arity2_funcs()) do
          2
        else
          1
        end
      end
    end
  end



  def types_arity1_funcs() do
    ["singleSubst", "mkTVar", "mkTCon0", "tArray", "tMaybe", "tSet", "tList", "tTuple"]
  end



  def types_arity2_funcs() do
    ["composeSubst", "applySubst", "mkTCon", "tArrow", "tEither", "tMap", "extendEnv", "lookupEnv", "freshVar", "generalize", "instantiate"]
  end



  def is_unify_module_func(name) do
    Nova.Array.elem(name, ["unify", "unifyMany", "bindVar", "occurs", "unifyRecords", "unifyField"])
  end



  def unify_module_func_arity(name) do
    case name do
      "unify" -> 2
      "unifyMany" -> 2
      "bindVar" -> 2
      "occurs" -> 2
      "unifyRecords" -> 2
      "unifyField" -> 4
      _ -> 2
    end
  end



  def translate_qualified(mod_, name) do
    case {:tuple, mod_, name} do
      {:tuple, "Int", "fromString"} -> "Nova.String.to_int"
      {:tuple, "Data.Int", "fromString"} -> "Nova.String.to_int"
      {:tuple, "Number", "fromString"} -> "Nova.String.to_float"
      {:tuple, "Data.Number", "fromString"} -> "Nova.String.to_float"
      _ -> 
          elixir_mod = case mod_ do
            "Map" -> "Nova.Map"
            "Data.Map" -> "Nova.Map"
            "Set" -> "Nova.Set"
            "Data.Set" -> "Nova.Set"
            "Array" -> "Nova.Array"
            "Data.Array" -> "Nova.Array"
            "List" -> "Nova.List"
            "Data.List" -> "Nova.List"
            "String" -> "Nova.String"
            "Data.String" -> "Nova.String"
            "Data.String.CodeUnits" -> "Nova.String"
            "SCU" -> "Nova.String"
            "CU" -> "Nova.String"
            "Ast" -> "Nova.Compiler.Ast"
            "Nova.Compiler.Ast" -> "Nova.Compiler.Ast"
            "Types" -> "Nova.Compiler.Types"
            "Nova.Compiler.Types" -> "Nova.Compiler.Types"
            "Unify" -> "Nova.Compiler.Unify"
            "Nova.Compiler.Unify" -> "Nova.Compiler.Unify"
            "Tokenizer" -> "Nova.Compiler.Tokenizer"
            "Nova.Compiler.Tokenizer" -> "Nova.Compiler.Tokenizer"
            "Parser" -> "Nova.Compiler.Parser"
            "Nova.Compiler.Parser" -> "Nova.Compiler.Parser"
            "TypeChecker" -> "Nova.Compiler.TypeChecker"
            "Nova.Compiler.TypeChecker" -> "Nova.Compiler.TypeChecker"
            "CodeGen" -> "Nova.Compiler.CodeGen"
            "Nova.Compiler.CodeGen" -> "Nova.Compiler.CodeGen"
            _ -> elixir_module_name(mod_)
          end
          Nova.Runtime.append(Nova.Runtime.append(elixir_mod, "."), snake_case(name))
    end
  end



  def gen_partial_app(func_name, applied_args, num_args, arity) do
    
      remaining = (arity - num_args)
      extra_params = Nova.Runtime.map((fn i -> Nova.Runtime.append("auto_p", Nova.Runtime.show(i)) end), (Nova.Array.range(0, ((remaining - 1)))))
      curried_header = Nova.Runtime.intercalate(" ", (Nova.Runtime.map((fn p -> Nova.Runtime.append(Nova.Runtime.append("fn ", p), " ->") end), extra_params)))
      curried_ends = Nova.Runtime.intercalate("", (Nova.Runtime.map((fn _ -> " end" end), extra_params)))
      all_args_str = if (num_args == 0) do
        Nova.Runtime.intercalate(", ", extra_params)
      else
        Nova.Runtime.append(Nova.Runtime.append(applied_args, ", "), Nova.Runtime.intercalate(", ", extra_params))
      end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(curried_header, " "), func_name), "("), all_args_str), ")"), curried_ends)
  end



  def gen_constructor_app(ctx, indent, name, args) do
    
      gen_args = Nova.Runtime.map((fn auto_p0 -> gen_expr_prime(ctx, indent, auto_p0) end), args)
      ast_prefix = if is_ast_constructor(name) do
        "Nova.Compiler.Ast."
      else
        ""
      end
      default_gen = if is_ast_constructor(name) do
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ast_prefix, snake_case(name)), "("), Nova.Runtime.intercalate(", ", gen_args)), ")")
      else
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:", snake_case(name)), ", "), Nova.Runtime.intercalate(", ", gen_args)), "}")
      end
      default_gen_no_args = if is_ast_constructor(name) do
        Nova.Runtime.append(Nova.Runtime.append(ast_prefix, snake_case(name)), "()")
      else
        Nova.Runtime.append(":", snake_case(name))
      end
      case name do
  "Tuple" -> case gen_args do
      [a, b] -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:tuple, ", a), ", "), b), "}")
      _ -> Nova.Runtime.append(Nova.Runtime.append("{:tuple, ", Nova.Runtime.intercalate(", ", gen_args)), "}")
    end
  "Tuple2" -> Nova.Runtime.append(Nova.Runtime.append("{:tuple, ", Nova.Runtime.intercalate(", ", gen_args)), "}")
  "Tuple3" -> Nova.Runtime.append(Nova.Runtime.append("{:tuple3, ", Nova.Runtime.intercalate(", ", gen_args)), "}")
  "Tuple4" -> Nova.Runtime.append(Nova.Runtime.append("{:tuple4, ", Nova.Runtime.intercalate(", ", gen_args)), "}")
  "Tuple5" -> Nova.Runtime.append(Nova.Runtime.append("{:tuple5, ", Nova.Runtime.intercalate(", ", gen_args)), "}")
  "Just" -> Nova.Runtime.append(Nova.Runtime.append("{:just, ", Nova.Runtime.intercalate(", ", gen_args)), "}")
  "Nothing" -> ":nothing"
  "Left" -> Nova.Runtime.append(Nova.Runtime.append("{:left, ", Nova.Runtime.intercalate(", ", gen_args)), "}")
  "Right" -> Nova.Runtime.append(Nova.Runtime.append("{:right, ", Nova.Runtime.intercalate(", ", gen_args)), "}")
  "Cons" -> case gen_args do
      [h, t] -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("[", h), " | "), t), "]")
      _ -> Nova.Runtime.append(Nova.Runtime.append("Nova.Runtime.cons(", Nova.Runtime.intercalate(", ", gen_args)), ")")
    end
  "Nil" -> "[]"
  _ -> if Nova.Array.null(gen_args) do
      default_gen_no_args
    else
      default_gen
    end
end
  end



  def gen_expr_prime(ctx, _, ({:expr_var, name})) do
    if Nova.Set.member(name, ctx.locals) do
      snake_case(name)
    else
      case name do
        "Nothing" -> ":nothing"
        "nothing" -> ":nothing"
        "Nil" -> "[]"
        "otherwise" -> "true"
        "True" -> "true"
        "False" -> "false"
        "not" -> "(&Kernel.not/1)"
        "mod" -> "(&rem/2)"
        "__guarded__" -> ":__guarded__"
        _ -> if Nova.String.contains((Nova.String.pattern(".")), name) do
            
              parts = Nova.String.split((Nova.String.pattern(".")), name)
              len = Nova.Array.length(parts)
              if (len > 1) do
  
    mod_parts = Nova.Array.take(((len - 1)), parts)
    func_name = case Nova.Array.last(parts) do
      {:just, n} -> n
      :nothing -> name
    end
    Nova.Runtime.append(Nova.Runtime.append("(&", translate_qualified((Nova.Runtime.intercalate(".", mod_parts)), func_name)), "/2)")
else
  snake_case(name)
end
          else
            if is_nullary_constructor(name) do
              Nova.Runtime.append(":", snake_case(name))
            else
              if is_module_func(ctx, name) do
                case lookup_arity(name, ctx) do
                  {:just, 0} -> Nova.Runtime.append(snake_case(name), "()")
                  {:just, arity} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(&", snake_case(name)), "/"), Nova.Runtime.show(arity)), ")")
                  :nothing -> Nova.Runtime.append(Nova.Runtime.append("(&", snake_case(name)), "/1)")
                end
              else
                if is_types_module_func(name) do
                  
                    arity = types_module_func_arity(name)
                    if (arity == 0) do
  Nova.Runtime.append(Nova.Runtime.append("Nova.Compiler.Types.", snake_case(name)), "()")
else
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(&Nova.Compiler.Types.", snake_case(name)), "/"), Nova.Runtime.show(arity)), ")")
end
                else
                  if is_unify_module_func(name) do
                    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(&Nova.Compiler.Unify.", snake_case(name)), "/"), Nova.Runtime.show((unify_module_func_arity(name)))), ")")
                  else
                    if is_prelude_func(name) do
                      Nova.Runtime.append(Nova.Runtime.append("(&Nova.Runtime.", snake_case(name)), "/1)")
                    else
                      if is_ast_constructor(name) do
                        Nova.Runtime.append(Nova.Runtime.append("(&Nova.Compiler.Ast.", snake_case(name)), "/1)")
                      else
                        if (is_data_constructor(name) and not((is_nullary_constructor(name)))) do
                          Nova.Runtime.append(Nova.Runtime.append("(&", snake_case(name)), "/1)")
                        else
                          snake_case(name)
                        end
                      end
                    end
                  end
                end
              end
            end
          end
      end
    end
  end

  def gen_expr_prime(_, _, ({:expr_qualified, mod_, name})) do
    if is_ast_constructor(name) do
      
        arity = get_ast_constructor_arity(name)
        if (arity == 0) do
  Nova.Runtime.append(translate_qualified(mod_, name), "()")
else
  if (arity == 1) do
    Nova.Runtime.append(Nova.Runtime.append("fn a -> ", translate_qualified(mod_, name)), "(a) end")
  else
    Nova.Runtime.append(Nova.Runtime.append("fn a, b -> ", translate_qualified(mod_, name)), "(a, b) end")
  end
end
    else
      if is_types_module_func(name) do
        
          arity = types_module_func_arity(name)
          if (arity == 0) do
  Nova.Runtime.append(Nova.Runtime.append("Nova.Compiler.Types.", snake_case(name)), "()")
else
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(&Nova.Compiler.Types.", snake_case(name)), "/"), Nova.Runtime.show(arity)), ")")
end
      else
        translate_qualified(mod_, name)
      end
    end
  end

  def gen_expr_prime(_, _, ({:expr_lit, lit})) do
    gen_literal(lit)
  end

  def gen_expr_prime(ctx, indent, ({:expr_app, f, arg})) do
    
      gen_var_app = fn c -> fn i -> fn n -> fn exprs -> fn args_s -> if (n == "not") do
        Nova.Runtime.append(Nova.Runtime.append("not(", args_s), ")")
      else
        if (n == "mod") do
          Nova.Runtime.append(Nova.Runtime.append("rem(", args_s), ")")
        else
          if Nova.String.contains((Nova.String.pattern(".")), n) do
            
              parts = Nova.String.split((Nova.String.pattern(".")), n)
              len = Nova.Array.length(parts)
              if (len > 1) do
  
    mod_parts = Nova.Array.take(((len - 1)), parts)
    func_name = case Nova.Array.last(parts) do
      {:just, fn_} -> fn_
      :nothing -> n
    end
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(translate_qualified((Nova.Runtime.intercalate(".", mod_parts)), func_name), "("), args_s), ")")
else
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(snake_case(n), ".("), args_s), ")")
end
          else
            if is_data_constructor(n) do
              gen_constructor_app(c, i, n, exprs)
            else
              if is_module_func(c, n) do
                case lookup_arity(n, c) do
                  {:just, arity} -> 
                      num_args = Nova.Runtime.length(exprs)
                      if (num_args == arity) do
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(snake_case(n), "("), args_s), ")")
else
  if (num_args < arity) do
    gen_partial_app((snake_case(n)), args_s, num_args, arity)
  else
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(snake_case(n), "("), args_s), ")")
  end
end
                  :nothing -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(snake_case(n), "("), args_s), ")")
                end
              else
                if is_types_module_func(n) do
                  
                    arity = types_module_func_arity(n)
                    num_args = Nova.Runtime.length(exprs)
                    if (num_args == arity) do
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("Nova.Compiler.Types.", snake_case(n)), "("), args_s), ")")
else
  if (num_args < arity) do
    gen_partial_app((Nova.Runtime.append("Nova.Compiler.Types.", snake_case(n))), args_s, num_args, arity)
  else
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("Nova.Compiler.Types.", snake_case(n)), "("), args_s), ")")
  end
end
                else
                  if is_unify_module_func(n) do
                    
                      arity = unify_module_func_arity(n)
                      num_args = Nova.Runtime.length(exprs)
                      if (num_args == arity) do
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("Nova.Compiler.Unify.", snake_case(n)), "("), args_s), ")")
else
  if (num_args < arity) do
    gen_partial_app((Nova.Runtime.append("Nova.Compiler.Unify.", snake_case(n))), args_s, num_args, arity)
  else
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("Nova.Compiler.Unify.", snake_case(n)), "("), args_s), ")")
  end
end
                  else
                    if is_prelude_func(n) do
                      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("Nova.Runtime.", snake_case(n)), "("), args_s), ")")
                    else
                      gen_chained_app((snake_case(n)), exprs, c, i)
                    end
                  end
                end
              end
            end
          end
        end
      end end end end end end
      
  %{func: func, args: args} = collect_args((Nova.Compiler.Ast.expr_app(f, arg)))
  gen_arg = fn a -> gen_expr_prime(ctx, indent, a) end
  args_str = Nova.Runtime.intercalate(", ", (Nova.Runtime.map(gen_arg, args)))
  case func do
  {:expr_var, name} -> gen_var_app.(ctx).(indent).(name).(args).(args_str)
  {:expr_qualified, m, n} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(translate_qualified(m, n), "("), args_str), ")")
  {:expr_lambda, _, _} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(", gen_expr_prime(ctx, indent, func)), ").("), args_str), ")")
  _ -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(", gen_expr_prime(ctx, indent, func)), ").("), args_str), ")")
end
  end

  def gen_expr_prime(ctx, indent, ({:expr_lambda, pats, body})) do
    
      gen_curried_lambda = Nova.Runtime.fix4(fn gen_curried_lambda -> fn c -> fn i -> fn ps -> fn b -> case Nova.List.uncons(ps) do
        :nothing -> gen_expr_prime(c, i, b)
        {:just, %{head: p, tail: rest}} -> if Nova.List.null(rest) do
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fn ", gen_pattern(p)), " -> "), gen_expr_prime(c, i, b)), " end")
          else
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fn ", gen_pattern(p)), " -> "), gen_curried_lambda.(c).(i).(rest).(b)), " end")
          end
      end  end end end end end)
      
  ctx_with_params = Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, pats)
  gen_curried_lambda.(ctx_with_params).(indent).(pats).(body)
  end

  def gen_expr_prime(ctx, indent, ({:expr_let, binds, body})) do
    
      ctx_with_binds = Nova.Runtime.foldr((fn b -> fn c -> add_locals_from_pattern(b.pattern, c) end end), ctx, binds)
      grouped_binds = group_binds_by_name((Nova.Array.from_foldable(binds)))
      sorted_groups = sort_groups_by_dependencies(grouped_binds)
      bind_code = Nova.Runtime.intercalate("\n", (Nova.Runtime.map((fn auto_p0 -> gen_binding_group(ctx, ((indent + 1)), auto_p0) end), sorted_groups)))
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("\n", bind_code), "\n"), ind(((indent + 1)))), gen_expr_prime(ctx_with_binds, 0, body))
  end

  def gen_expr_prime(ctx, indent, ({:expr_if, cond_, then_, else_})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("if ", gen_expr_prime(ctx, indent, cond_)), " do\n"), gen_expr_ctx(ctx, ((indent + 1)), then_)), "\n"), ind(indent)), "else\n"), gen_expr_ctx(ctx, ((indent + 1)), else_)), "\n"), ind(indent)), "end")
  end

  def gen_expr_prime(ctx, indent, ({:expr_case, scrutinee, clauses})) do
    
      grouped_clauses = group_wildcard_guarded_clauses((Nova.Array.from_foldable(clauses)))
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", gen_expr_prime(ctx, indent, scrutinee)), " do\n"), Nova.Runtime.intercalate("\n", (Nova.Runtime.map((fn auto_p0 -> gen_case_clause_group(ctx, ((indent + 1)), auto_p0) end), grouped_clauses)))), "\n"), ind(indent)), "end")
  end

  def gen_expr_prime(ctx, indent, ({:expr_do, stmts})) do
    gen_do_stmts_ctx(ctx, indent, (Nova.Array.from_foldable(stmts)))
  end

  def gen_expr_prime(ctx, _, ({:expr_bin_op, ":", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("[", gen_expr_prime(ctx, 0, l)), " | "), gen_expr_prime(ctx, 0, r)), "]")
  end

  def gen_expr_prime(ctx, _, ({:expr_bin_op, "<>", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("Nova.Runtime.append(", gen_expr_prime(ctx, 0, l)), ", "), gen_expr_prime(ctx, 0, r)), ")")
  end

  def gen_expr_prime(ctx, _, ({:expr_bin_op, "<<<", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fn auto_c -> (", gen_expr_prime(ctx, 0, l)), ").(("), gen_expr_prime(ctx, 0, r)), ").(auto_c)) end")
  end

  def gen_expr_prime(ctx, _, ({:expr_bin_op, ">>>", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fn auto_c -> (", gen_expr_prime(ctx, 0, r)), ").(("), gen_expr_prime(ctx, 0, l)), ").(auto_c)) end")
  end

  def gen_expr_prime(ctx, _, ({:expr_bin_op, "$", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(", gen_expr_prime(ctx, 0, l)), ").("), gen_expr_prime(ctx, 0, r)), ")")
  end

  def gen_expr_prime(ctx, _, ({:expr_bin_op, "#", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(", gen_expr_prime(ctx, 0, r)), ").("), gen_expr_prime(ctx, 0, l)), ")")
  end

  def gen_expr_prime(ctx, _, ({:expr_bin_op, "<$>", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("Nova.Runtime.fmap(", gen_expr_prime(ctx, 0, l)), ", "), gen_expr_prime(ctx, 0, r)), ")")
  end

  def gen_expr_prime(ctx, _, ({:expr_bin_op, "<|>", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("Nova.Runtime.alt(", gen_expr_prime(ctx, 0, l)), ", "), gen_expr_prime(ctx, 0, r)), ")")
  end

  def gen_expr_prime(ctx, _, ({:expr_bin_op, "*>", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("Nova.Runtime.seq(", gen_expr_prime(ctx, 0, l)), ", "), gen_expr_prime(ctx, 0, r)), ")")
  end

  def gen_expr_prime(ctx, _, ({:expr_bin_op, "<*", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("Nova.Runtime.seq_left(", gen_expr_prime(ctx, 0, l)), ", "), gen_expr_prime(ctx, 0, r)), ")")
  end

  def gen_expr_prime(ctx, _, ({:expr_bin_op, op, l, r})) do
    
      is_upper_case = fn s -> case Nova.String.char_at(0, s) do
        {:just, c} -> ((c >= ?A) and (c <= ?Z))
        :nothing -> false
      end end
      
  is_underscore = fn e -> case e do
    {:expr_var, "_"} -> true
    {:expr_section, "_"} -> true
    _ -> false
  end end
  case l do
  _ ->
    cond do
      is_underscore.(l) -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fn __x__ -> (__x__ ", gen_bin_op(op)), " "), gen_expr_prime(ctx, 0, r)), ") end")
      true -> case r do
      _ ->
        cond do
          is_underscore.(r) -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fn __x__ -> (", gen_expr_prime(ctx, 0, l)), " "), gen_bin_op(op)), " __x__) end")
          true -> if (Nova.String.contains((Nova.String.pattern(".")), op) or is_upper_case.((Nova.String.take(1, op)))) do
          
            func_call = if Nova.String.contains((Nova.String.pattern(".")), op) do
              
                parts = Nova.String.split((Nova.String.pattern(".")), op)
                len = Nova.Array.length(parts)
                if (len > 1) do
  
    mod_parts = Nova.Array.take(((len - 1)), parts)
    func_name = case Nova.Array.last(parts) do
      {:just, n} -> n
      :nothing -> op
    end
    translate_qualified((Nova.Runtime.intercalate(".", mod_parts)), func_name)
else
  snake_case(op)
end
            else
              snake_case(op)
            end
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(func_call, "("), gen_expr_prime(ctx, 0, l)), ", "), gen_expr_prime(ctx, 0, r)), ")")
        else
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(", gen_expr_prime(ctx, 0, l)), " "), gen_bin_op(op)), " "), gen_expr_prime(ctx, 0, r)), ")")
        end
        end
      _ -> if (Nova.String.contains((Nova.String.pattern(".")), op) or is_upper_case.((Nova.String.take(1, op)))) do
          
            func_call = if Nova.String.contains((Nova.String.pattern(".")), op) do
              
                parts = Nova.String.split((Nova.String.pattern(".")), op)
                len = Nova.Array.length(parts)
                if (len > 1) do
  
    mod_parts = Nova.Array.take(((len - 1)), parts)
    func_name = case Nova.Array.last(parts) do
      {:just, n} -> n
      :nothing -> op
    end
    translate_qualified((Nova.Runtime.intercalate(".", mod_parts)), func_name)
else
  snake_case(op)
end
            else
              snake_case(op)
            end
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(func_call, "("), gen_expr_prime(ctx, 0, l)), ", "), gen_expr_prime(ctx, 0, r)), ")")
        else
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(", gen_expr_prime(ctx, 0, l)), " "), gen_bin_op(op)), " "), gen_expr_prime(ctx, 0, r)), ")")
        end
    end
    end
  _ -> case r do
      _ ->
        cond do
          is_underscore.(r) -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fn __x__ -> (", gen_expr_prime(ctx, 0, l)), " "), gen_bin_op(op)), " __x__) end")
          true -> if (Nova.String.contains((Nova.String.pattern(".")), op) or is_upper_case.((Nova.String.take(1, op)))) do
          
            func_call = if Nova.String.contains((Nova.String.pattern(".")), op) do
              
                parts = Nova.String.split((Nova.String.pattern(".")), op)
                len = Nova.Array.length(parts)
                if (len > 1) do
  
    mod_parts = Nova.Array.take(((len - 1)), parts)
    func_name = case Nova.Array.last(parts) do
      {:just, n} -> n
      :nothing -> op
    end
    translate_qualified((Nova.Runtime.intercalate(".", mod_parts)), func_name)
else
  snake_case(op)
end
            else
              snake_case(op)
            end
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(func_call, "("), gen_expr_prime(ctx, 0, l)), ", "), gen_expr_prime(ctx, 0, r)), ")")
        else
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(", gen_expr_prime(ctx, 0, l)), " "), gen_bin_op(op)), " "), gen_expr_prime(ctx, 0, r)), ")")
        end
        end
      _ -> if (Nova.String.contains((Nova.String.pattern(".")), op) or is_upper_case.((Nova.String.take(1, op)))) do
          
            func_call = if Nova.String.contains((Nova.String.pattern(".")), op) do
              
                parts = Nova.String.split((Nova.String.pattern(".")), op)
                len = Nova.Array.length(parts)
                if (len > 1) do
  
    mod_parts = Nova.Array.take(((len - 1)), parts)
    func_name = case Nova.Array.last(parts) do
      {:just, n} -> n
      :nothing -> op
    end
    translate_qualified((Nova.Runtime.intercalate(".", mod_parts)), func_name)
else
  snake_case(op)
end
            else
              snake_case(op)
            end
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(func_call, "("), gen_expr_prime(ctx, 0, l)), ", "), gen_expr_prime(ctx, 0, r)), ")")
        else
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(", gen_expr_prime(ctx, 0, l)), " "), gen_bin_op(op)), " "), gen_expr_prime(ctx, 0, r)), ")")
        end
    end
end
  end

  def gen_expr_prime(ctx, _, ({:expr_unary_op, op, e})) do
    Nova.Runtime.append(gen_unary_op(op), gen_expr_prime(ctx, 0, e))
  end

  def gen_expr_prime(ctx, _, ({:expr_list, elems})) do
    Nova.Runtime.append(Nova.Runtime.append("[", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> gen_expr_prime(ctx, 0, auto_p0) end), elems)))))), "]")
  end

  def gen_expr_prime(ctx, _, ({:expr_tuple, elems})) do
    Nova.Runtime.append(Nova.Runtime.append("{", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> gen_expr_prime(ctx, 0, auto_p0) end), elems)))))), "}")
  end

  def gen_expr_prime(ctx, _, ({:expr_record, fields})) do
    
      gen_record_field = fn ({:tuple, label, expr}) -> Nova.Runtime.append(Nova.Runtime.append(snake_case(label), ": "), gen_expr_prime(ctx, 0, expr)) end
      Nova.Runtime.append(Nova.Runtime.append("%{", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map(gen_record_field, fields)))))), "}")
  end

  def gen_expr_prime(ctx, _, ({:expr_record_access, rec, field})) do
    
      collect_record_access_chain = Nova.Runtime.fix(fn collect_record_access_chain -> fn auto_arg0 -> case auto_arg0 do
        ({:expr_record_access, inner, f}) -> 
  result = collect_record_access_chain.(inner)
  %{result | fields: Nova.Array.snoc(result.fields, f)}
        e -> %{base: e, fields: []}
      end end end)
      case rec do
  {:expr_var, "_"} -> Nova.Runtime.append("& &1.", snake_case(field))
  {:expr_section, "_"} -> Nova.Runtime.append("& &1.", snake_case(field))
  {:expr_record_access, _inner, _inner_field} -> case collect_record_access_chain.(rec) do
      %{base: {:expr_var, "_"}, fields: fields} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("& &1.", Nova.Runtime.intercalate(".", (Nova.Runtime.map((&snake_case/1), fields)))), "."), snake_case(field))
      %{base: {:expr_section, "_"}, fields: fields} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("& &1.", Nova.Runtime.intercalate(".", (Nova.Runtime.map((&snake_case/1), fields)))), "."), snake_case(field))
      _ -> Nova.Runtime.append(Nova.Runtime.append(gen_expr_prime(ctx, 0, rec), "."), snake_case(field))
    end
  _ -> Nova.Runtime.append(Nova.Runtime.append(gen_expr_prime(ctx, 0, rec), "."), snake_case(field))
end
  end

  def gen_expr_prime(ctx, _, ({:expr_record_update, rec, fields})) do
    
      gen_update_field = fn ({:tuple, label, expr}) -> Nova.Runtime.append(Nova.Runtime.append(snake_case(label), ": "), gen_expr_prime(ctx, 0, expr)) end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("%{", gen_expr_prime(ctx, 0, rec)), " | "), Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map(gen_update_field, fields)))))), "}")
  end

  def gen_expr_prime(ctx, indent, ({:expr_typed, e, _})) do
    gen_expr_prime(ctx, indent, e)
  end

  def gen_expr_prime(ctx, indent, ({:expr_parens, e})) do
    Nova.Runtime.append(Nova.Runtime.append("(", gen_expr_prime(ctx, indent, e)), ")")
  end

  def gen_expr_prime(_, _, ({:expr_section, op})) do
    case Nova.String.strip_prefix((Nova.String.pattern(".")), op) do
      {:just, field} -> Nova.Runtime.append("& &1.", snake_case(field))
      :nothing -> Nova.Runtime.append(Nova.Runtime.append("fn __x__, __y__ -> (__x__ ", gen_bin_op(op)), " __y__) end")
    end
  end

  def gen_expr_prime(ctx, _, ({:expr_section_left, expr, op})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fn __x__ -> (", gen_expr_prime(ctx, 0, expr)), " "), gen_bin_op(op)), " __x__) end")
  end

  def gen_expr_prime(ctx, _, ({:expr_section_right, op, expr})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fn __x__ -> (__x__ ", gen_bin_op(op)), " "), gen_expr_prime(ctx, 0, expr)), ") end")
  end



  def gen_literal(({:lit_int, n})) do
    Nova.Runtime.show(n)
  end

  def gen_literal(({:lit_number, n})) do
    Nova.Runtime.show(n)
  end

  def gen_literal(({:lit_string, s})) do
    Nova.Runtime.append(Nova.Runtime.append("\"", escape_string(s)), "\"")
  end

  def gen_literal(({:lit_char, c})) do
    case c do
      ?\n -> "?\\n"
      ?\r -> "?\\r"
      ?\t -> "?\\t"
      ?\\ -> "?\\\\"
      ?\s -> "?\\s"
      _ -> Nova.Runtime.append("?", Nova.String.singleton(c))
    end
  end

  def gen_literal(({:lit_bool, true})) do
    "true"
  end

  def gen_literal(({:lit_bool, false})) do
    "false"
  end



  def contains_var(name, ({:expr_var, v})) do
    (v == name)
  end

  def contains_var(name, ({:expr_app, f, a})) do
    (contains_var(name, f) or contains_var(name, a))
  end

  def contains_var(name, ({:expr_lambda, _, body})) do
    contains_var(name, body)
  end

  def contains_var(name, ({:expr_let, binds, body})) do
    (Nova.List.any((fn b -> contains_var(name, b.value) end), binds) or contains_var(name, body))
  end

  def contains_var(name, ({:expr_if, c, t, e})) do
    ((contains_var(name, c) or contains_var(name, t)) or contains_var(name, e))
  end

  def contains_var(name, ({:expr_case, scrut, clauses})) do
    
      contains_var_in_maybe_expr = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {n, :nothing} -> false
        {n, ({:just, e})} -> contains_var(n, e)
      end end end
      (contains_var(name, scrut) or Nova.List.any((fn cl -> (contains_var(name, cl.body) or contains_var_in_maybe_expr.(name).(cl.guard)) end), clauses))
  end

  def contains_var(name, ({:expr_do, stmts})) do
    Nova.List.any((fn auto_p0 -> contains_var_in_do_stmt(name, auto_p0) end), stmts)
  end

  def contains_var(name, ({:expr_bin_op, _, l, r})) do
    (contains_var(name, l) or contains_var(name, r))
  end

  def contains_var(name, ({:expr_unary_op, _, e})) do
    contains_var(name, e)
  end

  def contains_var(name, ({:expr_list, es})) do
    Nova.List.any((fn auto_p0 -> contains_var(name, auto_p0) end), es)
  end

  def contains_var(name, ({:expr_tuple, es})) do
    Nova.List.any((fn auto_p0 -> contains_var(name, auto_p0) end), es)
  end

  def contains_var(name, ({:expr_record, fs})) do
    Nova.List.any((fn ({:tuple, _, e}) -> contains_var(name, e) end), fs)
  end

  def contains_var(name, ({:expr_record_access, e, _})) do
    contains_var(name, e)
  end

  def contains_var(name, ({:expr_record_update, e, fs})) do
    (contains_var(name, e) or Nova.List.any((fn ({:tuple, _, ex}) -> contains_var(name, ex) end), fs))
  end

  def contains_var(name, ({:expr_typed, e, _})) do
    contains_var(name, e)
  end

  def contains_var(name, ({:expr_parens, e})) do
    contains_var(name, e)
  end

  def contains_var(_, _) do
    false
  end



  def contains_var_in_do_stmt(name, ({:do_let, binds})) do
    Nova.List.any((fn b -> contains_var(name, b.value) end), binds)
  end

  def contains_var_in_do_stmt(name, ({:do_bind, _, e})) do
    contains_var(name, e)
  end

  def contains_var_in_do_stmt(name, ({:do_expr, e})) do
    contains_var(name, e)
  end



  def lambda_arity(({:expr_lambda, pats, _})) do
    Nova.List.length(pats)
  end

  def lambda_arity(_) do
    0
  end



  def get_bind_name(bind) do
    case bind.pattern do
      {:pat_var, n} -> {:just, n}
      _ -> :nothing
    end
  end



  def group_binds_by_name(binds) do
    
      go = Nova.Runtime.fix2(fn go -> fn arr -> fn acc -> case Nova.Array.uncons(arr) do
        :nothing -> acc
        {:just, %{head: b, tail: rest}} -> 
            name = get_bind_name(b)
            case name do
  :nothing -> go.(rest).((Nova.Array.snoc(acc, [b])))
  {:just, n} -> 
      spanned = Nova.Array.span((fn b_prime -> (get_bind_name(b_prime) == {:just, n}) end), rest)
      same = spanned.init
      different = spanned.rest
      group = Nova.Array.cons(b, same)
      go.(different).((Nova.Array.snoc(acc, group)))
end
      end  end end end)
      go.(binds).([])
  end



  def sort_groups_by_dependencies(groups) do
    
      group_name = fn grp -> case Nova.Array.head(grp) do
        :nothing -> :nothing
        {:just, b} -> get_bind_name(b)
      end end
      all_names = Nova.Array.map_maybe(group_name, groups)
      group_deps = fn grp -> 
        self_name = group_name.(grp)
        used_names = Nova.Array.concat_map((fn b -> get_used_vars(b.value) end), grp)
        Nova.Array.filter((fn n -> (({:just, n} != self_name) and Nova.Array.elem(n, all_names)) end), used_names) end
      group_info = Nova.Runtime.map((fn g -> %{group: g, name: group_name.(g), deps: group_deps.(g)} end), groups)
      topo_sort_bind_groups(all_names, group_info, [])
  end



  def topo_sort_bind_groups(all_names, infos, resolved) do
    if Nova.Array.null(infos) do
      resolved
    else
      
        resolved_names = Nova.Array.map_maybe((&extract_bind_group_name/1), resolved)
        partitioned = Nova.Array.partition((fn auto_p0 -> can_resolve_group(resolved_names, infos, auto_p0) end), infos)
        can_resolve = partitioned.yes
        remaining = partitioned.no
        if Nova.Array.null(can_resolve) do
  Nova.Runtime.append(resolved, Nova.Runtime.map(& &1.group, remaining))
else
  topo_sort_bind_groups(all_names, remaining, (Nova.Runtime.append(resolved, Nova.Runtime.map(& &1.group, can_resolve))))
end
    end
  end

  # @type bind_group_info :: %{group: array()(let_bind()), name: maybe()(string()), deps: array()(string())}



  def extract_bind_group_name(g) do
    case Nova.Array.head(g) do
      :nothing -> :nothing
      {:just, b} -> get_bind_name(b)
    end
  end



  def can_resolve_group(resolved_names, infos, info) do
    Nova.Array.all((fn auto_p0 -> dep_resolved(resolved_names, infos, auto_p0) end), info.deps)
  end



  def dep_resolved(resolved_names, infos, d) do
    (Nova.Array.elem(d, resolved_names) or not((Nova.Array.any((fn auto_p0 -> has_name(d, auto_p0) end), infos))))
  end



  def has_name(d, info) do
    (info.name == {:just, d})
  end



  def get_used_vars(({:expr_var, n})) do
    [n]
  end

  def get_used_vars(({:expr_app, f, a})) do
    Nova.Runtime.append(get_used_vars(f), get_used_vars(a))
  end

  def get_used_vars(({:expr_lambda, _, body})) do
    get_used_vars(body)
  end

  def get_used_vars(({:expr_let, binds, body})) do
    Nova.Runtime.append(list_concat_map((fn b -> get_used_vars(b.value) end), binds), get_used_vars(body))
  end

  def get_used_vars(({:expr_if, c, t, e})) do
    Nova.Runtime.append(Nova.Runtime.append(get_used_vars(c), get_used_vars(t)), get_used_vars(e))
  end

  def get_used_vars(({:expr_case, scrut, clauses})) do
    Nova.Runtime.append(get_used_vars(scrut), list_concat_map((fn cl -> get_used_vars(cl.body) end), clauses))
  end

  def get_used_vars(({:expr_bin_op, _, l, r})) do
    Nova.Runtime.append(get_used_vars(l), get_used_vars(r))
  end

  def get_used_vars(({:expr_list, elems})) do
    list_concat_map((&get_used_vars/1), elems)
  end

  def get_used_vars(({:expr_record, fields})) do
    list_concat_map((fn ({:tuple, _, v}) -> get_used_vars(v) end), fields)
  end

  def get_used_vars(({:expr_record_access, rec, _})) do
    get_used_vars(rec)
  end

  def get_used_vars(({:expr_record_update, rec, fields})) do
    Nova.Runtime.append(get_used_vars(rec), list_concat_map((fn ({:tuple, _, v}) -> get_used_vars(v) end), fields))
  end

  def get_used_vars(({:expr_do, stmts})) do
    
      get_used_vars_stmt = fn auto_arg0 -> case auto_arg0 do
        ({:do_let, binds}) -> list_concat_map((fn b -> get_used_vars(b.value) end), binds)
        ({:do_bind, _, e}) -> get_used_vars(e)
        ({:do_expr, e}) -> get_used_vars(e)
      end end
      list_concat_map_array(get_used_vars_stmt, stmts)
  end

  def get_used_vars(({:expr_tuple, elems})) do
    list_concat_map((&get_used_vars/1), elems)
  end

  def get_used_vars(({:expr_typed, e, _})) do
    get_used_vars(e)
  end

  def get_used_vars(({:expr_parens, e})) do
    get_used_vars(e)
  end

  def get_used_vars(({:expr_section, _})) do
    []
  end

  def get_used_vars(({:expr_section_left, e, _})) do
    get_used_vars(e)
  end

  def get_used_vars(({:expr_section_right, _, e})) do
    get_used_vars(e)
  end

  def get_used_vars(({:expr_qualified, _, _})) do
    []
  end

  def get_used_vars(({:expr_unary_op, _, e})) do
    get_used_vars(e)
  end

  def get_used_vars(_) do
    []
  end



  def list_concat_map(f, lst) do
    Nova.Runtime.foldl((fn acc -> fn x -> Nova.Runtime.append(acc, f.(x)) end end), [], lst)
  end



  def list_concat_map_array(f, lst) do
    Nova.Runtime.foldl((fn acc -> fn x -> Nova.Runtime.append(acc, f.(x)) end end), [], lst)
  end



  def gen_binding_group(ctx, indent, binds) do
    case binds do
      [] -> ""
      [single] -> gen_let_bind_ctx(ctx, indent, single)
      _ -> case Nova.Array.head(binds) do
          :nothing -> ""
          {:just, first_bind} -> case get_bind_name(first_bind) do
              :nothing -> Nova.Runtime.intercalate("\n", (Nova.Runtime.map((fn auto_p0 -> gen_let_bind_ctx(ctx, indent, auto_p0) end), binds)))
              {:just, name} -> 
                  clauses = Nova.Array.map_maybe((&extract_lambda_clause/1), binds)
                  is_recursive = Nova.Array.any((fn b -> contains_var(name, b.value) end), binds)
                  arity = case Nova.Array.head(clauses) do
                    :nothing -> 0
                    {:just, c} -> Nova.Runtime.length(c.patterns)
                  end
                  if (Nova.Array.null(clauses) or (arity == 0)) do
  Nova.Runtime.intercalate("\n", (Nova.Runtime.map((fn auto_p0 -> gen_let_bind_ctx(ctx, indent, auto_p0) end), binds)))
else
  gen_merged_function(ctx, indent, name, arity, clauses, is_recursive)
end
            end
        end
    end
  end



  def extract_lambda_clause(bind) do
    case bind.value do
      {:expr_lambda, pats, body} -> {:just, %{patterns: Nova.Array.from_foldable(pats), body: body}}
      _ -> :nothing
    end
  end



  def gen_merged_function(ctx, indent, name, arity, clauses, is_recursive) do
    
      arg_names = Nova.Runtime.map((fn i -> Nova.Runtime.append("auto_arg", Nova.Runtime.show(i)) end), (Nova.Array.range(0, ((arity - 1)))))
      case_body = gen_merged_case_clauses(ctx, ((indent + 1)), arity, clauses)
      fix_fn = case arity do
        1 -> "Nova.Runtime.fix"
        2 -> "Nova.Runtime.fix2"
        3 -> "Nova.Runtime.fix3"
        4 -> "Nova.Runtime.fix4"
        _ -> "Nova.Runtime.fix5"
      end
      args_str = Nova.Runtime.intercalate(", ", arg_names)
      curried_lambda_header = Nova.Runtime.intercalate(" ", (Nova.Runtime.map((fn a -> Nova.Runtime.append(Nova.Runtime.append("fn ", a), " ->") end), arg_names)))
      curried_lambda_ends = Nova.Runtime.intercalate("", (Nova.Runtime.map((fn _ -> " end" end), arg_names)))
      scrutinee = if (arity == 1) do
        (Nova.Runtime.from_maybe("auto_arg0")).(Nova.Array.head(arg_names))
      else
        Nova.Runtime.append(Nova.Runtime.append("{", args_str), "}")
      end
      if is_recursive do
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), snake_case(name)), " = "), fix_fn), "(fn "), snake_case(name)), " -> "), curried_lambda_header), " case "), scrutinee), " do\n"), case_body), "\n"), ind(indent)), "end"), curried_lambda_ends), " end)")
else
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), snake_case(name)), " = "), curried_lambda_header), " case "), scrutinee), " do\n"), case_body), "\n"), ind(indent)), "end"), curried_lambda_ends)
end
  end



  def gen_merged_case_clauses(ctx, indent, arity, clauses) do
    
      gen_clause = fn clause -> 
        pat_str = if (arity == 1) do
          case Nova.Array.head(clause.patterns) do
            :nothing -> "_"
            {:just, p} -> gen_pattern(p)
          end
        else
          Nova.Runtime.append(Nova.Runtime.append("{", Nova.Runtime.intercalate(", ", (Nova.Runtime.map((&gen_pattern/1), clause.patterns)))), "}")
        end
        ctx_with_pats = Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, clause.patterns)
        body_str = gen_expr_prime(ctx_with_pats, 0, clause.body)
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), pat_str), " -> "), body_str) end
      Nova.Runtime.intercalate("\n", (Nova.Runtime.map(gen_clause, clauses)))
  end



  def get_bind_dependencies(all_names, bind) do
    
      self_name = get_bind_name(bind)
      Nova.Array.filter((fn name -> (({:just, name} != self_name) and contains_var(name, bind.value)) end), all_names)
  end



  def sort_binds_by_dependencies(binds) do
    
      all_names = Nova.Array.map_maybe((&get_bind_name/1), binds)
      bind_info = Nova.Runtime.map((fn auto_p0 -> mk_bind_info(all_names, auto_p0) end), binds)
      topo_sort_let_binds(bind_info, [])
  end



  def mk_bind_info(all_names, b) do
    %{bind: b, name: get_bind_name(b), deps: get_bind_dependencies(all_names, b)}
  end

  # @type let_bind_info :: %{bind: let_bind(), name: maybe()(string()), deps: array()(string())}



  def topo_sort_let_binds(infos, resolved) do
    if Nova.Array.null(infos) do
      resolved
    else
      
        resolved_names = Nova.Array.map_maybe((&get_bind_name/1), resolved)
        partitioned = Nova.Array.partition((fn auto_p0 -> can_resolve_let_bind(resolved_names, infos, auto_p0) end), infos)
        can_resolve = partitioned.yes
        remaining = partitioned.no
        if Nova.Array.null(can_resolve) do
  Nova.Runtime.append(resolved, Nova.Runtime.map(& &1.bind, remaining))
else
  topo_sort_let_binds(remaining, (Nova.Runtime.append(resolved, Nova.Runtime.map(& &1.bind, can_resolve))))
end
    end
  end



  def can_resolve_let_bind(resolved_names, infos, info) do
    Nova.Array.all((fn auto_p0 -> let_dep_resolved(resolved_names, infos, auto_p0) end), info.deps)
  end



  def let_dep_resolved(resolved_names, infos, d) do
    (Nova.Array.elem(d, resolved_names) or not((is_bind_info_name(d, infos))))
  end



  def is_bind_info_name(name, infos) do
    Nova.Array.any((fn auto_p0 -> has_info_name(name, auto_p0) end), infos)
  end



  def has_info_name(name, info) do
    (info.name == {:just, name})
  end



  def gen_let_bind() do
    fn auto_p0 -> fn auto_p1 -> gen_let_bind_ctx(empty_ctx(), auto_p0, auto_p1) end end
  end



  def gen_let_bind_ctx(ctx, indent, bind) do
    
      var_name = case bind.pattern do
        {:pat_var, n} -> {:just, n}
        _ -> :nothing
      end
      arity = lambda_arity(bind.value)
      is_recursive = case var_name do
        {:just, n} -> contains_var(n, bind.value)
        :nothing -> false
      end
      case %{is_recursive: is_recursive, var_name: var_name, value: bind.value} do
  %{is_recursive: true, var_name: {:just, name}, value: {:expr_lambda, pats, body}} -> 
      fix_fn = case arity do
        1 -> "Nova.Runtime.fix"
        2 -> "Nova.Runtime.fix2"
        3 -> "Nova.Runtime.fix3"
        4 -> "Nova.Runtime.fix4"
        _ -> "Nova.Runtime.fix5"
      end
      ctx_with_params = Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, pats)
      curried_lambda_header = Nova.Runtime.intercalate(" ", (Nova.Array.from_foldable((Nova.Runtime.map((fn p -> Nova.Runtime.append(Nova.Runtime.append("fn ", gen_pattern(p)), " ->") end), pats)))))
      curried_lambda_ends = Nova.Runtime.intercalate("", (Nova.Array.from_foldable((Nova.Runtime.map((fn _ -> " end" end), pats)))))
      body_code = gen_expr_prime(ctx_with_params, indent, body)
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), snake_case(name)), " = "), fix_fn), "(fn "), snake_case(name)), " -> "), curried_lambda_header), " "), body_code), " "), curried_lambda_ends), " end)")
  _ -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), gen_pattern(bind.pattern)), " = "), gen_expr_prime(ctx, indent, bind.value))
end
  end



  def gen_case_clause() do
    fn auto_p0 -> fn auto_p1 -> gen_case_clause_ctx(empty_ctx(), auto_p0, auto_p1) end end
  end



  def is_guard_safe(({:expr_var, name})) do
    is_guard_safe_var(name)
  end

  def is_guard_safe(({:expr_lit, _})) do
    true
  end

  def is_guard_safe(({:expr_bin_op, op, l, r})) do
    ((is_guard_safe_op(op) and is_guard_safe(l)) and is_guard_safe(r))
  end

  def is_guard_safe(({:expr_unary_op, "not", e})) do
    is_guard_safe(e)
  end

  def is_guard_safe(({:expr_parens, e})) do
    is_guard_safe(e)
  end

  def is_guard_safe(({:expr_app, ({:expr_var, f}), arg})) do
    (is_guard_safe_func(f) and is_guard_safe(arg))
  end

  def is_guard_safe(({:expr_app, ({:expr_app, ({:expr_var, f}), arg1}), arg2})) do
    ((is_guard_safe_func(f) and is_guard_safe(arg1)) and is_guard_safe(arg2))
  end

  def is_guard_safe(_) do
    false
  end



  def is_guard_safe_var(_) do
    true
  end



  def is_guard_safe_op(op) do
    Nova.Array.elem(op, ["==", "/=", "<", ">", "<=", ">=", "&&", "||", "+", "-", "*", "/", "and", "or"])
  end



  def is_guard_safe_func(name) do
    Nova.Array.elem(name, ["is_atom", "is_binary", "is_bitstring", "is_boolean", "is_float", "is_function", "is_integer", "is_list", "is_map", "is_nil", "is_number", "is_pid", "is_port", "is_reference", "is_tuple", "abs", "bit_size", "byte_size", "ceil", "div", "elem", "floor", "hd", "length", "map_size", "node", "rem", "round", "self", "tl", "trunc", "tuple_size"])
  end



  def contains_pattern_bind(({:expr_bin_op, "<-", _, _})) do
    true
  end

  def contains_pattern_bind(({:expr_bin_op, _, l, r})) do
    (contains_pattern_bind(l) or contains_pattern_bind(r))
  end

  def contains_pattern_bind(({:expr_parens, e})) do
    contains_pattern_bind(e)
  end

  def contains_pattern_bind(_) do
    false
  end



  def split_guard(({:expr_bin_op, "&&", l, r})) do
    
      l_result = split_guard(l)
      r_result = split_guard(r)
      %{pat_binds: Nova.Runtime.append(l_result.pat_binds, r_result.pat_binds), conds: Nova.Runtime.append(l_result.conds, r_result.conds)}
  end

  def split_guard(({:expr_bin_op, "<-", pat, expr})) do
    %{pat_binds: [%{pat: pat, expr: expr}], conds: []}
  end

  def split_guard(({:expr_parens, e})) do
    split_guard(e)
  end

  def split_guard(e) do
    %{pat_binds: [], conds: [e]}
  end



  def expr_to_pattern(({:expr_var, v})) do
    snake_case(v)
  end

  def expr_to_pattern(({:expr_app, ({:expr_var, "Just"}), arg})) do
    Nova.Runtime.append(Nova.Runtime.append("{:just, ", expr_to_pattern(arg)), "}")
  end

  def expr_to_pattern(({:expr_app, ({:expr_var, "Nothing"}), _})) do
    ":nothing"
  end

  def expr_to_pattern(({:expr_var, "Nothing"})) do
    ":nothing"
  end

  def expr_to_pattern(({:expr_app, ({:expr_var, "Right"}), arg})) do
    Nova.Runtime.append(Nova.Runtime.append("{:right, ", expr_to_pattern(arg)), "}")
  end

  def expr_to_pattern(({:expr_app, ({:expr_var, "Left"}), arg})) do
    Nova.Runtime.append(Nova.Runtime.append("{:left, ", expr_to_pattern(arg)), "}")
  end

  def expr_to_pattern(({:expr_app, ({:expr_app, ({:expr_var, "Tuple"}), a}), b})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:tuple, ", expr_to_pattern(a)), ", "), expr_to_pattern(b)), "}")
  end

  def expr_to_pattern(({:expr_app, f, arg})) do
    case f do
      {:expr_var, con} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:", snake_case(con)), ", "), expr_to_pattern(arg)), "}")
      {:expr_app, _, _} -> 
          %{func: func, args: args} = collect_args((Nova.Compiler.Ast.expr_app(f, arg)))
          case func do
  {:expr_var, con} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:", snake_case(con)), ", "), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((&expr_to_pattern/1), args)))), "}")
  _ -> "_"
end
      _ -> "_"
    end
  end

  def expr_to_pattern(({:expr_lit, l})) do
    gen_literal(l)
  end

  def expr_to_pattern(({:expr_record, fields})) do
    
      gen_record_fields = fn fs -> Nova.Runtime.append(Nova.Runtime.append("{", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((fn ({:tuple, k, v}) -> Nova.Runtime.append(Nova.Runtime.append(snake_case(k), ": "), expr_to_pattern(v)) end), fs)))))), "}") end
      Nova.Runtime.append("%", gen_record_fields.(fields))
  end

  def expr_to_pattern(({:expr_parens, e})) do
    expr_to_pattern(e)
  end

  def expr_to_pattern(_) do
    "_"
  end



  def group_wildcard_guarded_clauses(clauses) do
    group_guarded_go(clauses, [])
  end



  def group_guarded_go(cs, acc) do
    case Nova.Array.uncons(cs) do
      :nothing -> acc
      {:just, %{head: c, tail: rest}} -> if (clause_has_guard(c) and not((is_clause_guard_safe(c)))) do
          
            same_pat = fn auto_p0 -> same_clause_pattern(c.pattern, auto_p0) end
            spanned = Nova.Array.span((fn auto_p0 -> should_group(same_pat, auto_p0) end), rest)
            guarded = Nova.Array.cons(c, spanned.init)
            group_and_remaining = compute_group_and_remaining(c.pattern, guarded, spanned.rest)
            group_guarded_go(group_and_remaining.remaining, (Nova.Array.snoc(acc, group_and_remaining.group)))
        else
          group_guarded_go(rest, (Nova.Array.snoc(acc, [c])))
        end
    end
  end



  def should_group(same_pat, cl) do
    (clause_has_guard(cl) and same_pat.(cl.pattern))
  end



  def compute_group_and_remaining(orig_pat, guarded, rest_clauses) do
    case Nova.Array.uncons(rest_clauses) do
      {:just, %{head: next, tail: remaining}} -> if can_pat_be_fallback(orig_pat, next.pattern) do
          %{group: Nova.Array.snoc(guarded, next), remaining: remaining}
        else
          %{group: guarded, remaining: rest_clauses}
        end
      :nothing -> %{group: guarded, remaining: []}
    end
  end



  def clause_has_guard(clause) do
    case clause.guard do
      {:just, _} -> true
      :nothing -> false
    end
  end



  def is_clause_guard_safe(clause) do
    case clause.guard do
      {:just, g} -> is_guard_safe(g)
      :nothing -> true
    end
  end



  def can_pat_be_fallback(orig_pat, fallback_pat) do
    case fallback_pat do
      :pat_wildcard -> true
      _ -> same_clause_pattern(orig_pat, fallback_pat)
    end
  end



  def same_clause_pattern(p1, p2) do
    case {:tuple, p1, p2} do
      {:tuple, :pat_wildcard, :pat_wildcard} -> true
      {:tuple, ({:pat_var, _}), ({:pat_var, _})} -> true
      {:tuple, ({:pat_con, n1, _}), ({:pat_con, n2, _})} -> (n1 == n2)
      {:tuple, ({:pat_lit, _}), ({:pat_lit, _})} -> true
      _ -> false
    end
  end



  def gen_case_clause_group(ctx, indent, clauses) do
    case clauses do
      [] -> ""
      [single] -> gen_case_clause_ctx(ctx, indent, single)
      _ -> 
          last_clause = Nova.Array.last(clauses)
          has_wildcard_fallback = case last_clause do
            {:just, c} -> case c.pattern do
                :pat_wildcard -> true
                _ -> false
              end
            :nothing -> false
          end
          if has_wildcard_fallback do
  gen_with_wildcard_fallback(ctx, indent, clauses)
else
  gen_with_cond_only(ctx, indent, clauses)
end
    end
  end



  def gen_with_wildcard_fallback(ctx, indent, clauses) do
    
      last_clause = case Nova.Array.last(clauses) do
        {:just, c} -> c
        :nothing -> %{pattern: :pat_wildcard, guard: :nothing, body: Nova.Compiler.Ast.expr_lit((Nova.Compiler.Ast.lit_int(0)))}
      end
      init_clauses = Nova.Runtime.from_maybe([], (Nova.Array.init(clauses)))
      used_vars = Nova.Runtime.foldr((fn cl -> fn s -> Nova.Set.union((used_vars_in_clause(cl)), s) end end), Nova.Set.empty, clauses)
      first_clause = case Nova.Array.head(init_clauses) do
        {:just, c} -> c
        :nothing -> last_clause
      end
      fallback_body = gen_expr_prime(ctx, ((indent + 1)), last_clause.body)
      pat = gen_pattern_with_used(used_vars, first_clause.pattern)
      ctx_with_pat = add_locals_from_pattern(first_clause.pattern, ctx)
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), pat), " ->\n"), ind(((indent + 1)))), "cond do\n"), Nova.Runtime.intercalate("\n", (Nova.Runtime.map((fn auto_p0 -> gen_cond_clause(ctx_with_pat, ((indent + 2)), auto_p0) end), init_clauses)))), "\n"), ind(((indent + 2)))), "true -> "), fallback_body), "\n"), ind(((indent + 1)))), "end\n"), ind(indent)), "_ -> "), fallback_body)
  end



  def gen_with_cond_only(ctx, indent, clauses) do
    
      first_clause = case Nova.Array.head(clauses) do
        {:just, c} -> c
        :nothing -> %{pattern: :pat_wildcard, guard: :nothing, body: Nova.Compiler.Ast.expr_lit((Nova.Compiler.Ast.lit_int(0)))}
      end
      used_vars = Nova.Runtime.foldr((fn cl -> fn s -> Nova.Set.union((used_vars_in_clause(cl)), s) end end), Nova.Set.empty, clauses)
      pat = gen_pattern_with_used(used_vars, first_clause.pattern)
      ctx_with_pat = add_locals_from_pattern(first_clause.pattern, ctx)
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), pat), " ->\n"), ind(((indent + 1)))), "cond do\n"), Nova.Runtime.intercalate("\n", (Nova.Runtime.map((fn auto_p0 -> gen_cond_clause(ctx_with_pat, ((indent + 2)), auto_p0) end), clauses)))), "\n"), ind(((indent + 1)))), "end")
  end



  def gen_cond_clause(ctx, indent, clause) do
    
      body = gen_expr_prime(ctx, ((indent + 1)), clause.body)
      case clause.guard do
  {:just, g} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), gen_expr_prime(ctx, 0, g)), " -> "), body)
  :nothing -> Nova.Runtime.append(Nova.Runtime.append(ind(indent), "true -> "), body)
end
  end



  def gen_case_clause_ctx(ctx, indent, clause) do
    
      add_bind_vars_from_expr = Nova.Runtime.fix2(fn add_bind_vars_from_expr -> fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {({:expr_var, v}), c} -> %{c | locals: Nova.Set.insert(v, c.locals)}
        {({:expr_app, f, arg}), c} -> add_bind_vars_from_expr.(f).((add_bind_vars_from_expr.(arg).(c)))
        {({:expr_record, fields}), c} -> Nova.Runtime.foldr((fn ({:tuple, _, e}) -> fn acc -> add_bind_vars_from_expr.(e).(acc) end end), c, fields)
        {({:expr_parens, e}), c} -> add_bind_vars_from_expr.(e).(c)
        {_, c} -> c
      end end end end)
      add_bind_vars = fn %{pat: pat} -> fn c -> add_bind_vars_from_expr.(pat).(c) end end
      
  ctx_with_pat = add_locals_from_pattern(clause.pattern, ctx)
  body_vars = free_vars_expr(clause.body)
  guard_vars = case clause.guard do
    :nothing -> Nova.Set.empty
    {:just, g} -> free_vars_expr(g)
  end
  used_vars = Nova.Set.union(body_vars, guard_vars)
  body = gen_expr_prime(ctx_with_pat, ((indent + 1)), clause.body)
  pat = gen_pattern_with_used(used_vars, clause.pattern)
  case clause.guard do
  :nothing -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), pat), " -> "), body)
  {:just, g} -> if contains_pattern_bind(g) do
      
        %{pat_binds: pat_binds, conds: conds} = split_guard(g)
        ctx_with_binds = Nova.Runtime.foldr(add_bind_vars, ctx_with_pat, pat_binds)
        body_with_binds = gen_expr_prime(ctx_with_binds, ((indent + 1)), clause.body)
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), pat), " ->\n"), gen_pattern_bind_chain(ctx_with_pat, ((indent + 1)), pat_binds, conds, body_with_binds))
    else
      if is_guard_safe(g) do
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), pat), " when "), gen_expr_prime(ctx_with_pat, indent, g)), " -> "), body)
      else
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), pat), " ->\n"), ind(((indent + 1)))), "if "), gen_expr_prime(ctx_with_pat, indent, g)), " do\n"), ind(((indent + 2)))), body), "\n"), ind(((indent + 1)))), "end")
      end
    end
end
  end



  def gen_pattern_bind_chain(ctx, indent, pat_binds, conds, body) do
    
      add_bind_vars_from_expr = Nova.Runtime.fix2(fn add_bind_vars_from_expr -> fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {({:expr_var, v}), c} -> %{c | locals: Nova.Set.insert(v, c.locals)}
        {({:expr_app, f, arg}), c} -> add_bind_vars_from_expr.(f).((add_bind_vars_from_expr.(arg).(c)))
        {({:expr_record, fields}), c} -> Nova.Runtime.foldr((fn ({:tuple, _, e}) -> fn acc -> add_bind_vars_from_expr.(e).(acc) end end), c, fields)
        {({:expr_parens, e}), c} -> add_bind_vars_from_expr.(e).(c)
        {_, c} -> c
      end end end end)
      case Nova.Array.uncons(pat_binds) do
  :nothing -> if Nova.Array.null(conds) do
      body
    else
      
        cond_expr = Nova.Array.foldl((fn acc -> fn c -> Nova.Compiler.Ast.expr_bin_op("&&", acc, c) end end), (Nova.Runtime.from_maybe((Nova.Compiler.Ast.expr_lit((Nova.Compiler.Ast.lit_bool(true)))), (Nova.Array.head(conds)))), (Nova.Runtime.from_maybe([], (Nova.Array.tail(conds)))))
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), "if "), gen_expr_prime(ctx, indent, cond_expr)), " do\n"), ind(((indent + 1)))), body), "\n"), ind(indent)), "end")
    end
  {:just, %{head: pb, tail: rest_binds}} -> 
      pat_str = expr_to_pattern(pb.pat)
      expr_str = gen_expr_prime(ctx, indent, pb.expr)
      ctx_with_bind = add_bind_vars_from_expr.(pb.pat).(ctx)
      inner_code = gen_pattern_bind_chain(ctx_with_bind, ((indent + 1)), rest_binds, conds, body)
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind(indent), "case "), expr_str), " do\n"), ind(((indent + 1)))), pat_str), " -> "), inner_code), "\n"), ind(((indent + 1)))), "_ -> nil\n"), ind(indent)), "end")
end
  end



  def gen_do_stmts() do
    fn auto_p0 -> fn auto_p1 -> gen_do_stmts_ctx(empty_ctx(), auto_p0, auto_p1) end end
  end



  def gen_do_stmts_ctx(ctx, indent, stmts) do
    case Nova.Array.uncons(stmts) do
      :nothing -> "nil"
      {:just, %{head: stmt, tail: rest}} -> case stmt do
          {:do_expr, e} -> 
              ind_str = repeat_str(indent, " ")
              if Nova.Array.null(rest) do
  Nova.Runtime.append(ind_str, gen_expr_prime(ctx, 0, e))
else
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind_str, gen_expr_prime(ctx, 0, e)), "\n"), gen_do_stmts_ctx(ctx, indent, rest))
end
          {:do_let, binds} -> 
              ctx_with_binds = Nova.Runtime.foldr((fn b -> fn c -> add_locals_from_pattern(b.pattern, c) end end), ctx, binds)
              Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.intercalate("\n", (Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> gen_let_bind_ctx(ctx, indent, auto_p0) end), binds))))), "\n"), gen_do_stmts_ctx(ctx_with_binds, indent, rest))
          {:do_bind, pat, e} -> 
              ctx_with_pat = add_locals_from_pattern(pat, ctx)
              monad_type = detect_monad_type(e)
              ind_str = repeat_str(indent, " ")
              pat_str = case pat do
                {:pat_con, "Tuple", ([p1 | ([p2 | []])])} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:tuple, ", gen_pattern(p1)), ", "), gen_pattern(p2)), "}")
                _ -> gen_pattern(pat)
              end
              if (monad_type == 0) do
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind_str, "case "), gen_expr_prime(ctx, 0, e)), " do\n"), ind_str), "  :nothing -> :nothing\n"), ind_str), "  {:just, "), pat_str), "} ->\n"), gen_do_stmts_ctx(ctx_with_pat, ((indent + 4)), rest)), "\n"), ind_str), "end")
else
  if (monad_type == 1) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind_str, "case "), gen_expr_prime(ctx, 0, e)), " do\n"), ind_str), "  {:left, err} -> {:left, err}\n"), ind_str), "  {:right, "), pat_str), "} ->\n"), gen_do_stmts_ctx(ctx_with_pat, ((indent + 4)), rest)), "\n"), ind_str), "end")
  else
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind_str, "Nova.Runtime.bind("), gen_expr_prime(ctx, 0, e)), ", fn "), pat_str), " ->\n"), gen_do_stmts_ctx(ctx_with_pat, ((indent + 2)), rest)), "\n"), ind_str), "end)")
  end
end
        end
    end
  end



  def detect_monad_type(expr) do
    
      is_maybe_func = fn name -> ((((((((((((((((name == "peek") or (name == "peekAt")) or (name == "charAt")) or (name == "head")) or (name == "tail")) or (name == "last")) or (name == "init")) or (name == "find")) or (name == "findIndex")) or (name == "elemIndex")) or (name == "lookup")) or (name == "index")) or (name == "uncons")) or (name == "fromString")) or (name == "stripPrefix")) or (name == "stripSuffix")) end
      is_either_func = fn name -> ((((((((((((Nova.String.contains((Nova.String.pattern("parse")), (Nova.String.to_lower(name))) or Nova.String.contains((Nova.String.pattern("unify")), (Nova.String.to_lower(name)))) or Nova.String.contains((Nova.String.pattern("expect")), (Nova.String.to_lower(name)))) or Nova.String.contains((Nova.String.pattern("collect")), (Nova.String.to_lower(name)))) or Nova.String.contains((Nova.String.pattern("infer")), (Nova.String.to_lower(name)))) or Nova.String.contains((Nova.String.pattern("check")), (Nova.String.to_lower(name)))) or Nova.String.contains((Nova.String.pattern("instantiate")), (Nova.String.to_lower(name)))) or Nova.String.contains((Nova.String.pattern("generalize")), (Nova.String.to_lower(name)))) or Nova.String.contains((Nova.String.pattern("lookup")), (Nova.String.to_lower(name)))) or Nova.String.contains((Nova.String.pattern("convert")), (Nova.String.to_lower(name)))) or (name == "traverse")) or (name == "success")) or (name == "failure")) end
      classify_func = fn name -> if is_maybe_func.(name) do
        0
      else
        if is_either_func.(name) do
          1
        else
          2
        end
      end end
      go = Nova.Runtime.fix(fn go -> fn auto_arg0 -> case auto_arg0 do
        ({:expr_var, name}) -> classify_func.(name)
        ({:expr_qualified, _, name}) -> classify_func.(name)
        ({:expr_app, f, _}) -> go.(f)
        ({:expr_parens, e}) -> go.(e)
        _ -> 2
      end end end)
      go.(expr)
  end



  def gen_data_type(dt) do
    
      gen_tuple_constructor = fn con -> 
        arity = Nova.List.length(con.fields)
        params = Nova.Array.map_with_index((fn i -> fn _ -> Nova.Runtime.append("arg", Nova.Runtime.show(i)) end end), (Nova.Array.from_foldable(con.fields)))
        args = Nova.Runtime.intercalate(", ", params)
        body = if (arity == 0) do
          Nova.Runtime.append(":", snake_case(con.name))
        else
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:", snake_case(con.name)), ", "), args), "}")
        end
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case(con.name)), "("), args), "), do: "), body) end
      gen_record_constructor = fn con -> 
        fields_arr = Nova.Array.from_foldable(con.fields)
        params = Nova.Runtime.map((fn f -> snake_case(f.label) end), fields_arr)
        fields = Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn f -> Nova.Runtime.append(Nova.Runtime.append(snake_case(f.label), ": "), snake_case(f.label)) end), fields_arr)))
        args = Nova.Runtime.intercalate(", ", params)
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case(con.name)), "("), args), "), do: %{__type__: :"), snake_case(con.name)), ", "), fields), "}") end
      gen_constructor = fn con -> if con.is_record do
        gen_record_constructor.(con)
      else
        gen_tuple_constructor.(con)
      end end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  # Data type: ", dt.name), "\n"), Nova.Runtime.intercalate("\n", (Nova.Array.from_foldable((Nova.Runtime.map(gen_constructor, dt.constructors))))))
  end



  def gen_newtype(nt) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  # Newtype: ", nt.name), "\n"), "  def "), snake_case(nt.constructor)), "(arg0), do: {:'"), snake_case(nt.constructor)), "', arg0}")
  end



  def gen_type_class(tc) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  # Type class: ", tc.name), " "), Nova.Runtime.intercalate(" ", (Nova.Array.from_foldable(tc.type_vars))))
  end



  def gen_type_class_instance(inst) do
    if inst.derived do
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  # derive instance ", inst.class_name), " "), gen_type_expr(inst.ty))
    else
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  # instance ", inst.class_name), " "), gen_type_expr(inst.ty))
    end
  end



  def gen_infix(inf) do
    
      assoc_str = case inf.associativity do
        :assoc_left -> "infixl"
        :assoc_right -> "infixr"
        :assoc_none -> "infix"
      end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  # ", assoc_str), " "), Nova.Runtime.show(inf.precedence)), " "), inf.function_name), " as "), inf.operator)
  end



  def gen_type_alias(ta) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  # @type ", snake_case(ta.name)), " :: "), gen_type_expr(ta.ty))
  end



  def gen_type_expr(({:ty_expr_con, name})) do
    Nova.Runtime.append(snake_case(name), "()")
  end

  def gen_type_expr(({:ty_expr_var, name})) do
    snake_case(name)
  end

  def gen_type_expr(({:ty_expr_app, f, arg})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(gen_type_expr(f), "("), gen_type_expr(arg)), ")")
  end

  def gen_type_expr(({:ty_expr_arrow, a, b})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(", gen_type_expr(a)), " -> "), gen_type_expr(b)), ")")
  end

  def gen_type_expr(({:ty_expr_record, fields, _})) do
    Nova.Runtime.append(Nova.Runtime.append("%{", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((fn ({:tuple, l, t}) -> Nova.Runtime.append(Nova.Runtime.append(snake_case(l), ": "), gen_type_expr(t)) end), fields)))))), "}")
  end

  def gen_type_expr(({:ty_expr_for_all, _, t})) do
    gen_type_expr(t)
  end

  def gen_type_expr(({:ty_expr_constrained, _, t})) do
    gen_type_expr(t)
  end

  def gen_type_expr(({:ty_expr_parens, t})) do
    Nova.Runtime.append(Nova.Runtime.append("(", gen_type_expr(t)), ")")
  end

  def gen_type_expr(({:ty_expr_tuple, ts})) do
    Nova.Runtime.append(Nova.Runtime.append("{", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((&gen_type_expr/1), ts)))))), "}")
  end



  def gen_bin_op("+") do
    "+"
  end

  def gen_bin_op("-") do
    "-"
  end

  def gen_bin_op("*") do
    "*"
  end

  def gen_bin_op("/") do
    "/"
  end

  def gen_bin_op("==") do
    "=="
  end

  def gen_bin_op("/=") do
    "!="
  end

  def gen_bin_op("<") do
    "<"
  end

  def gen_bin_op(">") do
    ">"
  end

  def gen_bin_op("<=") do
    "<="
  end

  def gen_bin_op(">=") do
    ">="
  end

  def gen_bin_op("&&") do
    "and"
  end

  def gen_bin_op("||") do
    "or"
  end

  def gen_bin_op("<>") do
    "<>"
  end

  def gen_bin_op("++") do
    "++"
  end

  def gen_bin_op(":") do
    "|"
  end

  def gen_bin_op(">>=") do
    "|> bind"
  end

  def gen_bin_op(op) do
    op
  end



  def gen_unary_op("-") do
    "-"
  end

  def gen_unary_op("not") do
    "not "
  end

  def gen_unary_op(op) do
    op
  end



  def snake_case(s) do
    
      handle_primes = fn str -> Nova.String.replace_all((Nova.String.pattern("'")), (Nova.String.replacement("_prime")), str) end
      go = Nova.Runtime.fix3(fn go -> fn auto_arg0 -> fn auto_arg1 -> fn auto_arg2 -> case {auto_arg0, auto_arg1, auto_arg2} do
        {[], _, acc} -> acc
        {cps, prev_lower, acc} -> case Nova.Array.uncons(cps) do
  :nothing -> acc
  {:just, %{head: cp, tail: rest}} -> 
      cp_str = Nova.String.singleton(cp)
      is_upper = ((cp_str >= "A") and (cp_str <= "Z"))
      lower = Nova.String.to_lower(cp_str)
      prefix = if (is_upper and prev_lower) do
        "_"
      else
        ""
      end
      go.(rest).((not(is_upper))).((Nova.Runtime.append(Nova.Runtime.append(acc, prefix), lower)))
end
      end end end end end)
      
  without_primes = handle_primes.(s)
  result = go.((Nova.String.to_code_point_array(without_primes))).(false).("")
  escape_reserved(result)
  end



  def escape_reserved(s) do
    
      elixir_reserved = ["nil", "true", "false", "do", "end", "if", "else", "unless", "case", "cond", "when", "and", "or", "not", "in", "fn", "def", "defp", "defmodule", "defstruct", "defmacro", "defimpl", "defprotocol", "defexception", "defdelegate", "defguard", "import", "require", "use", "alias", "for", "with", "quote", "unquote", "receive", "try", "catch", "rescue", "after", "raise", "throw", "exit", "super", "spawn", "send", "self", "mod", "rem", "div", "abs", "max", "min"]
      is_reserved = fn word -> Nova.Array.elem(word, elixir_reserved) end
      if is_reserved.(s) do
  Nova.Runtime.append(s, "_")
else
  s
end
  end



  def ind(n) do
    Nova.String.join_with("", (Nova.Array.replicate(((n * 2)), " ")))
  end



  def escape_string(s) do
    (Nova.String.replace_all((Nova.String.pattern("\t")), (Nova.String.replacement("\\t")))).((Nova.String.replace_all((Nova.String.pattern("\n")), (Nova.String.replacement("\\n")))).((Nova.String.replace_all((Nova.String.pattern("\"")), (Nova.String.replacement("\\\"")))).((Nova.String.replace_all((Nova.String.pattern("\\")), (Nova.String.replacement("\\\\")))).(s))))
  end
end
