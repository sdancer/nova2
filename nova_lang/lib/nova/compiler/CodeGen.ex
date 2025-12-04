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
    ctx.(%{local_arities: [%{name: name, arity: arity} | ctx.local_arities]})
  end



  def add_locals_from_pattern(({:pat_var, name}), ctx) do
    ctx.(%{locals: Nova.Set.insert(name, ctx.locals)})
  end

  def add_locals_from_pattern({:pat_wildcard, ctx}) do
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
    add_locals_from_pattern(pat, (ctx.(%{locals: Nova.Set.insert(name, ctx.locals)})))
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
      :nil_ -> Nova.Set.empty
      {:cons, ({:do_expr, e}), rest} -> Nova.Set.union((Nova.Set.difference((free_vars_expr(e)), bound)), (free_vars_do(rest, bound)))
      {:cons, ({:do_bind, pat, e}), rest} -> 
          expr_vars = Nova.Set.difference((free_vars_expr(e)), bound)
          new_bound = Nova.Set.union(bound, (pattern_vars(pat)))
          Nova.Set.union(expr_vars, (free_vars_do(rest, new_bound)))
      {:cons, ({:do_let, binds}), rest} -> 
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
    Nova.Runtime.foldr(go, Nova.Set.empty, decls)
  end



  def collect_func_arities(decls) do
    Nova.Array.map_maybe(go, decls)
  end



  def lookup_arity(name, ctx) do
    Nova.Runtime.map(& &1.arity, (Nova.Array.find((fn f -> (f.name == name) end), ctx.func_arities)))
  end



  def gen_module(mod_) do
    
      decls = Nova.Array.from_foldable(mod_.declarations)
      ctx = empty_ctx(%{module_funcs: collect_module_funcs(decls), func_arities: collect_func_arities(decls)})
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("defmodule ", elixir_module_name(mod_.name)), " do\n"), Nova.Runtime.intercalate("\n\n", (Nova.Runtime.map((fn auto_p0 -> gen_declaration(ctx, auto_p0) end), decls)))), "\nend\n")
  end



  def elixir_module_name(name) do
    Nova.String.replace_all((Nova.String.pattern(".")), (Nova.String.replacement(".")), name)
  end



  def gen_declaration(ctx, ({:decl_function, func})) do
    gen_function(ctx, func)
  end

  def gen_declaration(_, ({:decl_data_type, dt})) do
    gen_data_type.(dt)
  end

  def gen_declaration(_, ({:decl_newtype, nt})) do
    gen_newtype.(nt)
  end

  def gen_declaration(_, ({:decl_type_alias, ta})) do
    gen_type_alias.(ta)
  end

  def gen_declaration(_, ({:decl_import, imp})) do
    Nova.Runtime.append("  # import ", imp.module_name)
  end

  def gen_declaration(_, ({:decl_type_sig, _})) do
    ""
  end

  def gen_declaration(_, ({:decl_infix, inf})) do
    gen_infix.(inf)
  end

  def gen_declaration(_, ({:decl_foreign_import, fi})) do
    gen_foreign_import(fi)
  end

  def gen_declaration(_, ({:decl_type_class, tc})) do
    gen_type_class.(tc)
  end

  def gen_declaration(_, ({:decl_type_class_instance, inst})) do
    gen_type_class_instance.(inst)
  end

  def gen_declaration(_, _) do
    "  # unsupported declaration"
  end



  def gen_foreign_import(fi) do
    
      func_name = snake_case.(fi.function_name)
      ffi_module = case fi.module_name do
        "" -> "Nova.FFI"
        m -> Nova.Runtime.append("Nova.FFI.", m)
      end
      alias_name = case fi.alias_ do
        {:just, a} -> snake_case.(a)
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
          if List.null(func.guards) do
  
    body = gen_expr_ctx(ctx_with_params, 2, func.body)
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case.(func.name)), "("), params), ") do\n"), body), "\n"), "  end")
else
  
    body_code = gen_guarded_function_body(ctx_with_params, 2, (Nova.Array.from_foldable(func.guards)))
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case.(func.name)), "("), params), ") do\n"), body_code), "\n"), "  end")
end
    end
  end



  def handle_point_free_alias(ctx, func) do
    if not((List.null(func.parameters))) do
      :nothing
    else
      case func.body do
        {:expr_var, ref_name} -> case lookup_arity(ref_name, ctx) do
            {:just, arity} when (arity > 0) -> 
                arg_names = Nova.Runtime.map((fn i -> Nova.Runtime.append("auto_arg", Nova.Runtime.show(i)) end), (Nova.Array.range(0, ((arity - 1)))))
                args_str = Nova.Runtime.intercalate(", ", arg_names)
                {:just, (Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case.(func.name)), "("), args_str), ") do\n    "), snake_case.(ref_name)), "("), args_str), ")\n  end"))}
            _ -> :nothing
          end
        _ -> case func.body do
            {:expr_app, ({:expr_qualified, mod_, fn_}), arg} -> if is_partial_app_of_arity2(func.body) do
                
                  arg_code = gen_expr_ctx(ctx, 0, arg)
                  func_name = translate_qualified(mod_, fn_)
                  {:just, (Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case.(func.name)), "(auto_arg0) do\n    "), func_name), "("), arg_code), ", auto_arg0)\n  end"))}
              else
                :nothing
              end
            {:expr_app, ({:expr_var, fn_}), arg} -> if is_partial_app_of_arity2(func.body) do
                
                  arg_code = gen_expr_ctx(ctx, 0, arg)
                  {:just, (Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("  def ", snake_case.(func.name)), "(auto_arg0) do\n    "), snake_case.(fn_)), "("), arg_code), ", auto_arg0)\n  end"))}
              else
                :nothing
              end
            _ -> :nothing
          end
      end
    end
  end



  def is_partial_app_of_arity2(({:expr_app, ({:expr_qualified, "Array", fn_}), _})) do
    (((((((((((((((((fn_ == "filter") or fn_) == "map") or fn_) == "find") or fn_) == "any") or fn_) == "all") or fn_) == "takeWhile") or fn_) == "dropWhile") or fn_) == "sortBy") or fn_) == "groupBy")
  end

  def is_partial_app_of_arity2(({:expr_app, ({:expr_var, fn_}), _})) do
    (((((((((fn_ == "filter") or fn_) == "map") or fn_) == "find") or fn_) == "any") or fn_) == "all")
  end

  def is_partial_app_of_arity2(_) do
    false
  end



  def gen_guarded_function_body(ctx, indent, guards) do
    case Nova.Array.uncons(guards) do
      :nothing -> Nova.Runtime.append(ind.(indent), "nil")
      {:just, %{head: first_guard, tail: rest_guards}} -> 
          pat_guards = Nova.Array.filter(is_pat_guard, (Nova.Array.from_foldable(first_guard.guards)))
          expr_guards = Nova.Array.filter((fn g -> not((is_pat_guard.(g))) end), (Nova.Array.from_foldable(first_guard.guards)))
          if Nova.Array.null(pat_guards) do
  
    guard_clauses = Nova.Runtime.map((fn auto_p0 -> gen_guarded_expr_for_cond(ctx, ((indent + 1)), auto_p0) end), guards)
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind.(indent), "cond do\n"), Nova.Runtime.intercalate("\n", guard_clauses)), "\n"), ind.(indent)), "end")
else
  gen_pattern_guard_case(ctx, indent, first_guard, rest_guards)
end
    end
  end



  def gen_pattern_guard_case(ctx, indent, first_guard, rest_guards) do
    
      pat_guards = Nova.Array.filter(is_pat_guard, (Nova.Array.from_foldable(first_guard.guards)))
      expr_guards = Nova.Array.filter((fn g -> not((is_pat_guard.(g))) end), (Nova.Array.from_foldable(first_guard.guards)))
      case pat_guards do
  [{:guard_pat, pat, scrutinee_expr}] -> 
      scrutinee = gen_expr_ctx(ctx, 0, scrutinee_expr)
      pat_str = gen_pattern(pat)
      ctx_with_pat = add_locals_from_pattern(pat, ctx)
      fallthrough = if Nova.Array.null(rest_guards) do
        Nova.Runtime.append(ind.(((indent + 2))), "nil")
      else
        gen_guarded_function_body(ctx, ((indent + 2)), rest_guards)
      end
      body_expr = gen_expr_ctx(ctx_with_pat, 0, first_guard.body)
      when_clause = if Nova.Array.null(expr_guards) do
        ""
      else
        Nova.Runtime.append(" when ", gen_guard_clauses_simple(ctx_with_pat, expr_guards))
      end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind.(indent), "case "), scrutinee), " do\n"), ind.(((indent + 1)))), pat_str), when_clause), " ->\n"), ind.(((indent + 2)))), body_expr), "\n"), ind.(((indent + 1)))), "_ ->\n"), fallthrough), "\n"), ind.(indent)), "end")
  _ -> gen_nested_pattern_guards(ctx, indent, pat_guards, expr_guards, first_guard.body, rest_guards)
end
  end



  def gen_nested_pattern_guards(ctx, indent, pat_guards, expr_guards, body, rest_guards) do
    case Nova.Array.uncons(pat_guards) do
      :nothing -> if Nova.Array.null(expr_guards) do
          gen_expr_ctx(ctx, indent, body)
        else
          
            fallthrough = if Nova.Array.null(rest_guards) do
              Nova.Runtime.append(ind.(((indent + 1))), "nil")
            else
              gen_guarded_function_body(ctx, ((indent + 1)), rest_guards)
            end
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind.(indent), "if "), gen_guard_clauses_simple(ctx, expr_guards)), " do\n"), gen_expr_ctx(ctx, ((indent + 1)), body)), "\n"), ind.(indent)), "else\n"), fallthrough), "\n"), ind.(indent)), "end")
        end
      {:just, %{head: {:guard_pat, pat, scrutinee_expr}, tail: remaining_pat_guards}} -> 
          scrutinee = gen_expr_ctx(ctx, 0, scrutinee_expr)
          pat_str = gen_pattern(pat)
          ctx_with_pat = add_locals_from_pattern(pat, ctx)
          fallthrough = if Nova.Array.null(rest_guards) do
            Nova.Runtime.append(ind.(((indent + 1))), "_ -> nil")
          else
            Nova.Runtime.append(Nova.Runtime.append(ind.(((indent + 1))), "_ ->\n"), gen_guarded_function_body(ctx, ((indent + 2)), rest_guards))
          end
          inner_code = gen_nested_pattern_guards(ctx_with_pat, ((indent + 2)), remaining_pat_guards, expr_guards, body, rest_guards)
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind.(indent), "case "), scrutinee), " do\n"), ind.(((indent + 1)))), pat_str), " ->\n"), inner_code), "\n"), fallthrough), "\n"), ind.(indent)), "end")
      {:just, %{head: {:guard_expr, _}, tail: _}} -> gen_guarded_function_body(ctx, indent, rest_guards)
    end
  end



  def gen_guard_clauses_simple(ctx, clauses) do
    
      exprs = Nova.Array.map_maybe(extract_expr_guard, clauses)
      Nova.Runtime.intercalate(" and ", (Nova.Runtime.map((fn e -> gen_expr_ctx(ctx, 0, e) end), exprs)))
  end



  def gen_guarded_expr_for_cond(ctx, indent, ge) do
    
      expr_guards = Nova.Array.from_foldable(ge.guards)
      body_expr = gen_expr_ctx(ctx, 0, ge.body)
      guard_expr = gen_guard_clauses(ctx, expr_guards)
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ind.(indent), guard_expr), " ->\n"), ind.(((indent + 1)))), body_expr)
  end



  def gen_guarded_expr(ctx, indent, ge) do
    
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
    snake_case.(name)
  end

  def gen_pattern(:pat_wildcard) do
    "_"
  end

  def gen_pattern(({:pat_lit, lit})) do
    gen_literal.(lit)
  end

  def gen_pattern(({:pat_con, name, pats})) do
    
      con_name = case Nova.String.last_index_of((Nova.String.pattern(".")), name) do
        {:just, i} -> Nova.String.drop(((i + 1)), name)
        :nothing -> name
      end
      if List.null(pats) do
  Nova.Runtime.append(":", snake_case.(con_name))
else
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:", snake_case.(con_name)), ", "), Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((&gen_pattern/1), pats)))))), "}")
end
  end

  def gen_pattern(({:pat_record, fields})) do
    Nova.Runtime.append(Nova.Runtime.append("%{", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map(gen_field_pattern, fields)))))), "}")
  end

  def gen_pattern(({:pat_list, pats})) do
    Nova.Runtime.append(Nova.Runtime.append("[", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((&gen_pattern/1), pats)))))), "]")
  end

  def gen_pattern(({:pat_cons, head, tail})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("[", gen_pattern(head)), " | "), gen_pattern(tail)), "]")
  end

  def gen_pattern(({:pat_as, name, pat})) do
    Nova.Runtime.append(Nova.Runtime.append(gen_pattern(pat), " = "), snake_case.(name))
  end

  def gen_pattern(({:pat_parens, p})) do
    Nova.Runtime.append(Nova.Runtime.append("(", gen_pattern(p)), ")")
  end



  def gen_pattern_with_used(used, ({:pat_var, name})) do
    if Nova.Set.member(name, used) do
      snake_case.(name)
    else
      if (Nova.String.take(1, name) == "_") do
        "_"
      else
        Nova.Runtime.append("_", snake_case.(name))
      end
    end
  end

  def gen_pattern_with_used(_, :pat_wildcard) do
    "_"
  end

  def gen_pattern_with_used(_, ({:pat_lit, lit})) do
    gen_literal.(lit)
  end

  def gen_pattern_with_used(used, ({:pat_con, name, pats})) do
    
      con_name = case Nova.String.last_index_of((Nova.String.pattern(".")), name) do
        {:just, i} -> Nova.String.drop(((i + 1)), name)
        :nothing -> name
      end
      if List.null(pats) do
  Nova.Runtime.append(":", snake_case.(con_name))
else
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:", snake_case.(con_name)), ", "), Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> gen_pattern_with_used(used, auto_p0) end), pats)))))), "}")
end
  end

  def gen_pattern_with_used(used, ({:pat_record, fields})) do
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
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(gen_pattern_with_used(used, pat), " = "), prefix), snake_case.(name))
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
    go.(expr).([])
  end



  def is_module_func(ctx, name) do
    (Nova.Set.member(name, ctx.module_funcs) and not((Nova.Set.member(name, ctx.locals))))
  end



  def gen_expr() do
    fn auto_p0 -> fn auto_p1 -> gen_expr_ctx(empty_ctx(), auto_p0, auto_p1) end end
  end



  def gen_expr_ctx(ctx, indent, expr) do
    Nova.Runtime.append(ind.(indent), gen_expr_prime(ctx, indent, expr))
  end



  def is_data_constructor(name) do
    Nova.Array.elem(name, ["Tuple", "Tuple2", "Tuple3", "Tuple4", "Tuple5", "Just", "Nothing", "Left", "Right", "Cons", "Nil", "TyVar", "TyCon", "TyRecord", "TokKeyword", "TokIdentifier", "TokNumber", "TokString", "TokChar", "TokOperator", "TokDelimiter", "TokNewline", "TokUnrecognized", "ExprVar", "ExprLit", "ExprApp", "ExprLambda", "ExprLet", "ExprIf", "ExprCase", "ExprBinOp", "ExprList", "ExprRecord", "ExprRecordAccess", "ExprParens", "ExprDo", "ExprQualified", "ExprRecordUpdate", "ExprTyped", "ExprUnaryOp", "ExprTuple", "ExprSection", "ExprSectionLeft", "ExprSectionRight", "ExprNegate", "PatVar", "PatWildcard", "PatLit", "PatCon", "PatRecord", "PatList", "PatCons", "PatAs", "PatParens", "PatTyped", "LitInt", "LitString", "LitChar", "LitBool", "LitNumber", "DeclFunction", "DeclTypeSig", "DeclDataType", "DeclTypeAlias", "DeclModule", "DeclImport", "DeclTypeClass", "DeclTypeClassInstance", "DeclInfix", "DeclForeignImport", "DeclType", "TyExprCon", "TyExprVar", "TyExprApp", "TyExprArrow", "TyExprRecord", "TyExprForAll", "TyExprTuple", "TyExprConstrained", "TyExprParens", "OccursCheck", "TypeMismatch", "ArityMismatch", "RecordFieldMismatch", "UnifyErr", "UnboundVariable", "NotImplemented", "DoLet", "DoBind", "DoExpr", "GuardExpr", "GuardPat"])
  end



  def get_ast_constructor_arity(name) do
    if (((((name == "PatWildcard") or name) == "ImportAll") or name) == "ImportNone") do
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
    Nova.Array.elem(name, ["TokKeyword", "TokIdentifier", "TokNumber", "TokString", "TokChar", "TokOperator", "TokDelimiter", "TokNewline", "TokUnrecognized", "PatWildcard", "ImportAll", "ImportNone", "KindFunction", "KindDataType", "KindTypeAlias", "KindTypeClass", "KindInstance", "KindForeignImport", "AssocLeft", "AssocRight", "AssocNone"])
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
          Nova.Runtime.append(Nova.Runtime.append(elixir_mod, "."), snake_case.(name))
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
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(ast_prefix, snake_case.(name)), "("), Nova.Runtime.intercalate(", ", gen_args)), ")")
      else
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{:", snake_case.(name)), ", "), Nova.Runtime.intercalate(", ", gen_args)), "}")
      end
      default_gen_no_args = if is_ast_constructor(name) do
        Nova.Runtime.append(Nova.Runtime.append(ast_prefix, snake_case.(name)), "()")
      else
        Nova.Runtime.append(":", snake_case.(name))
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
      snake_case.(name)
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
  snake_case.(name)
end
          else
            if is_nullary_constructor(name) do
              Nova.Runtime.append(":", snake_case.(name))
            else
              if is_module_func(ctx, name) do
                case lookup_arity(name, ctx) do
                  {:just, 0} -> Nova.Runtime.append(snake_case.(name), "()")
                  {:just, arity} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(&", snake_case.(name)), "/"), Nova.Runtime.show(arity)), ")")
                  :nothing -> Nova.Runtime.append(Nova.Runtime.append("(&", snake_case.(name)), "/1)")
                end
              else
                if is_types_module_func(name) do
                  
                    arity = types_module_func_arity(name)
                    if (arity == 0) do
  Nova.Runtime.append(Nova.Runtime.append("Nova.Compiler.Types.", snake_case.(name)), "()")
else
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(&Nova.Compiler.Types.", snake_case.(name)), "/"), Nova.Runtime.show(arity)), ")")
end
                else
                  if is_unify_module_func(name) do
                    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(&Nova.Compiler.Unify.", snake_case.(name)), "/"), Nova.Runtime.show((unify_module_func_arity(name)))), ")")
                  else
                    if is_prelude_func(name) do
                      Nova.Runtime.append(Nova.Runtime.append("(&Nova.Runtime.", snake_case.(name)), "/1)")
                    else
                      if is_ast_constructor(name) do
                        Nova.Runtime.append(Nova.Runtime.append("(&Nova.Compiler.Ast.", snake_case.(name)), "/1)")
                      else
                        if (is_data_constructor(name) and not((is_nullary_constructor(name)))) do
                          Nova.Runtime.append(Nova.Runtime.append("(&", snake_case.(name)), "/1)")
                        else
                          snake_case.(name)
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
  Nova.Runtime.append(Nova.Runtime.append("Nova.Compiler.Types.", snake_case.(name)), "()")
else
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("(&Nova.Compiler.Types.", snake_case.(name)), "/"), Nova.Runtime.show(arity)), ")")
end
      else
        translate_qualified(mod_, name)
      end
    end
  end

  def gen_expr_prime(_, _, ({:expr_lit, lit})) do
    gen_literal.(lit)
  end
end
