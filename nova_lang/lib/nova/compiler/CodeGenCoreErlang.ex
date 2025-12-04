defmodule Nova.Compiler.CodeGenCoreErlang do
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

  # import Data.Map

  # import Data.Map

  # import Data.Foldable

  # import Nova.Compiler.Ast

  # @type core_ctx :: %{module_name: string(), module_funcs: set()(string()), locals: set()(string()), func_arities: array()(%{name: string(), arity: int()}), var_counter: int(), imports: map()(string())(string())}



  def empty_ctx(mod_name) do
    %{module_name: mod_name, module_funcs: Nova.Set.empty, locals: Nova.Set.empty, func_arities: [], var_counter: 0, imports: Nova.Map.empty}
  end

  # @type prelude_func_info :: %{mod_: string(), func: string(), arity: int()}



  def get_prelude_func("show") do
    {:just, %{mod_: "erlang", func: "integer_to_list", arity: 1}}
  end

  def get_prelude_func("foldl") do
    {:just, %{mod_: "functor", func: "foldl", arity: 3}}
  end

  def get_prelude_func("foldr") do
    {:just, %{mod_: "functor", func: "foldr", arity: 3}}
  end

  def get_prelude_func("map") do
    {:just, %{mod_: "functor", func: "map", arity: 2}}
  end

  def get_prelude_func("filter") do
    {:just, %{mod_: "lists", func: "filter", arity: 2}}
  end

  def get_prelude_func("length") do
    {:just, %{mod_: "erlang", func: "length", arity: 1}}
  end

  def get_prelude_func("reverse") do
    {:just, %{mod_: "lists", func: "reverse", arity: 1}}
  end

  def get_prelude_func("concat") do
    {:just, %{mod_: "lists", func: "concat", arity: 1}}
  end

  def get_prelude_func("head") do
    {:just, %{mod_: "erlang", func: "hd", arity: 1}}
  end

  def get_prelude_func("tail") do
    {:just, %{mod_: "erlang", func: "tl", arity: 1}}
  end

  def get_prelude_func("take") do
    {:just, %{mod_: "lists", func: "sublist", arity: 2}}
  end

  def get_prelude_func("drop") do
    {:just, %{mod_: "lists", func: "nthtail", arity: 2}}
  end

  def get_prelude_func("not") do
    {:just, %{mod_: "erlang", func: "not", arity: 1}}
  end

  def get_prelude_func("negate") do
    {:just, %{mod_: "erlang", func: "-", arity: 1}}
  end

  def get_prelude_func("mod") do
    {:just, %{mod_: "erlang", func: "rem", arity: 2}}
  end

  def get_prelude_func("pure") do
    :nothing
  end

  def get_prelude_func(_) do
    :nothing
  end



  def is_prelude_func(name) do
    case get_prelude_func(name) do
      {:just, _} -> true
      :nothing -> false
    end
  end



  def fresh_var(ctx) do
    %{var: Nova.Runtime.append("_cor", Nova.Runtime.show(ctx.var_counter)), ctx: ctx.(%{var_counter: (ctx.var_counter + 1)})}
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



  def free_vars_in_expr_for(candidates, bound, expr) do
    case expr do
      {:expr_var, name} -> if (Nova.Set.member(name, candidates) and not((Nova.Set.member(name, bound)))) do
          Nova.Set.singleton(name)
        else
          Nova.Set.empty
        end
      {:expr_lit, _} -> Nova.Set.empty
      {:expr_app, f, arg} -> Nova.Set.union((free_vars_in_expr_for(candidates, bound, f)), (free_vars_in_expr_for(candidates, bound, arg)))
      {:expr_lambda, pats, body} -> 
          pat_bound = Nova.Runtime.foldr(add_pattern_vars, bound, pats)
          free_vars_in_expr_for(candidates, pat_bound, body)
      {:expr_let, binds, body} -> 
          bind_names = Nova.Set.from_foldable((List.map_maybe((fn b -> get_pattern_var_name.(b.pattern) end), binds)))
          new_bound = Nova.Set.union(bound, bind_names)
          bind_vars = Nova.Runtime.foldr((fn b -> fn s -> Nova.Set.union(s, (free_vars_in_expr_for(candidates, new_bound, b.value))) end end), Nova.Set.empty, binds)
          Nova.Set.union(bind_vars, (free_vars_in_expr_for(candidates, new_bound, body)))
      {:expr_case, scrut, clauses} -> 
          scrut_vars = free_vars_in_expr_for(candidates, bound, scrut)
          clause_vars = Nova.Runtime.foldr((fn c -> fn s -> 
            clause_bound = add_pattern_vars.(c.pattern).(bound)
            guard_vars = case c.guard do
              :nothing -> Nova.Set.empty
              {:just, g} -> free_vars_in_expr_for(candidates, clause_bound, g)
            end
            body_vars = free_vars_in_expr_for(candidates, clause_bound, c.body)
            Nova.Set.union(s, (Nova.Set.union(guard_vars, body_vars))) end end), Nova.Set.empty, clauses)
          Nova.Set.union(scrut_vars, clause_vars)
      {:expr_if, cond_, thn, els} -> Nova.Set.union((free_vars_in_expr_for(candidates, bound, cond_)), (Nova.Set.union((free_vars_in_expr_for(candidates, bound, thn)), (free_vars_in_expr_for(candidates, bound, els)))))
      {:expr_do, _} -> Nova.Set.empty
      {:expr_list, exprs} -> Nova.Runtime.foldr((fn e -> fn s -> Nova.Set.union(s, (free_vars_in_expr_for(candidates, bound, e))) end end), Nova.Set.empty, exprs)
      {:expr_tuple, exprs} -> Nova.Runtime.foldr((fn e -> fn s -> Nova.Set.union(s, (free_vars_in_expr_for(candidates, bound, e))) end end), Nova.Set.empty, exprs)
      {:expr_record, fields} -> Nova.Runtime.foldr((fn ({:tuple, _, e}) -> fn s -> Nova.Set.union(s, (free_vars_in_expr_for(candidates, bound, e))) end end), Nova.Set.empty, fields)
      {:expr_record_access, e, _} -> free_vars_in_expr_for(candidates, bound, e)
      {:expr_record_update, e, updates} -> Nova.Set.union((free_vars_in_expr_for(candidates, bound, e)), (Nova.Runtime.foldr((fn ({:tuple, _, v}) -> fn s -> Nova.Set.union(s, (free_vars_in_expr_for(candidates, bound, v))) end end), Nova.Set.empty, updates)))
      {:expr_typed, e, _} -> free_vars_in_expr_for(candidates, bound, e)
      {:expr_bin_op, _, l, r} -> Nova.Set.union((free_vars_in_expr_for(candidates, bound, l)), (free_vars_in_expr_for(candidates, bound, r)))
      {:expr_unary_op, _, e} -> free_vars_in_expr_for(candidates, bound, e)
      {:expr_parens, e} -> free_vars_in_expr_for(candidates, bound, e)
      {:expr_section, _} -> Nova.Set.empty
      {:expr_section_left, e, _} -> free_vars_in_expr_for(candidates, bound, e)
      {:expr_section_right, _, e} -> free_vars_in_expr_for(candidates, bound, e)
      {:expr_qualified, _, _} -> Nova.Set.empty
    end
  end



  def topo_sort_binds(binds) do
    
      bind_map = Nova.Map.from_foldable((Nova.Array.map_maybe((fn b -> (get_pattern_var_name.(b.pattern) # Nova.Runtime.map((fn n -> {:tuple, n, b} end))) end), binds)))
      bind_names = Nova.Set.from_foldable((Nova.Map.keys(bind_map)))
      deps = Nova.Runtime.map((fn b -> 
        name = Nova.Runtime.from_maybe("", (get_pattern_var_name.(b.pattern)))
        free_vars = free_vars_in_expr_for(bind_names, Nova.Set.empty, b.value)
        local_deps = free_vars
        %{name: name, bind: b, deps: local_deps} end), binds)
      sorted = kahn_sort.(deps)
      sorted
  end



  def atom(s) do
    Nova.Runtime.append(Nova.Runtime.append("'", escape_atom.(s)), "'")
  end



  def core_var(name) do
    
      first = Nova.String.take(1, name)
      rest = Nova.String.drop(1, name)
      Nova.Runtime.append(Nova.String.to_upper(first), to_snake.(rest))
  end



  def gen_module(m) do
    
      mod_name = translate_module_name(m.name)
      all_funcs = Nova.Array.map_maybe(get_func, (Nova.Array.from_foldable(m.declarations)))
      all_imports = Nova.Array.concat_map(get_imports, (Nova.Array.from_foldable(m.declarations)))
      import_map = Nova.Map.from_foldable(all_imports)
      grouped = group_functions(all_funcs)
      unique_funcs = Nova.Array.nub_by_eq((fn a -> fn b -> (((a.name == b.name) and a.arity) == b.arity) end end), (Nova.Runtime.map((fn g -> %{name: g.name, arity: g.arity} end), grouped)))
      exports = Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn f -> Nova.Runtime.append(Nova.Runtime.append(atom(f.name), "/"), Nova.Runtime.show(f.arity)) end), unique_funcs)))
      ctx = ((empty_ctx(mod_name))).(%{module_funcs: Nova.Set.from_foldable((Nova.Runtime.map(& &1.name, unique_funcs))), func_arities: unique_funcs, imports: import_map})
      func_defs = Nova.Runtime.intercalate("\n\n", (Nova.Runtime.map((fn auto_p0 -> gen_function_group(ctx, auto_p0) end), grouped)))
      dt_comments = Nova.Runtime.intercalate("\n\n", (Nova.Array.map_maybe((gen_decl_non_func.(ctx)), (Nova.Array.from_foldable(m.declarations)))))
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("module ", atom(mod_name)), " ["), exports), "]\n"), "  attributes []\n"), dt_comments), (if (dt_comments == "") do
  ""
else
  "\n\n"
end)), func_defs), "\nend\n")
  end



  def import_items_to_tuples(mod_name, items) do
    Nova.Array.concat_map(to_tuples, items)
  end

  # @type function_group :: %{name: string(), arity: int(), clauses: array()(function_declaration())}



  def group_functions(funcs) do
    
      keys = Nova.Array.nub_by_eq((fn a -> fn b -> (((a.name == b.name) and a.arity) == b.arity) end end), (Nova.Runtime.map((fn f -> %{name: f.name, arity: List.length(f.parameters)} end), funcs)))
      mk_group = fn k -> %{name: k.name, arity: k.arity, clauses: Nova.Array.filter((fn f -> (((f.name == k.name) and List.length(f.parameters)) == k.arity) end), funcs)} end
      Nova.Runtime.map(mk_group, keys)
  end



  def gen_function_group(ctx, group) do
    case Nova.Array.uncons(group.clauses) do
      :nothing -> ""
      {:just, %{head: first, tail: []}} -> if has_complex_pattern(first.parameters) do
          gen_merged_function(ctx, group)
        else
          gen_function(ctx, first)
        end
      {:just, _} -> gen_merged_function(ctx, group)
    end
  end



  def has_complex_pattern(pats) do
    List.any(is_complex, pats)
  end



  def has_complex_pattern_single(({:pat_var, _})) do
    false
  end

  def has_complex_pattern_single(:pat_wildcard) do
    false
  end

  def has_complex_pattern_single(_) do
    true
  end



  def gen_merged_function(ctx, group) do
    
      arity = group.arity
      param_names = (Nova.Array.range(0, ((arity - 1))) # Nova.Runtime.map((fn i -> Nova.Runtime.append("_P", Nova.Runtime.show(i)) end)))
      params_str = Nova.Runtime.intercalate(", ", param_names)
      case_clauses = Nova.Runtime.map((fn auto_p0 -> gen_function_clause_as_case(ctx, param_names, auto_p0) end), group.clauses)
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(atom(group.name), "/"), Nova.Runtime.show(arity)), " =\n"), "  fun ("), params_str), ") ->\n"), "    case {"), params_str), "} of\n"), Nova.Runtime.intercalate("\n", case_clauses)), "\n"), "    end")
  end



  def gen_function_clause_as_case(ctx, _param_names, func) do
    
      pats_result = gen_pats_with_counter((Nova.Array.from_foldable(func.parameters)), 0)
      ctx_with_params = Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, func.parameters)
      pat_tuple = Nova.Runtime.append(Nova.Runtime.append("{", Nova.Runtime.intercalate(", ", pats_result.strs)), "}")
      body = if List.null(func.guards) do
        gen_expr(ctx_with_params, func.body)
      else
        gen_guarded_exprs(ctx_with_params, (Nova.Array.from_foldable(func.guards)))
      end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("      <", pat_tuple), "> when 'true' ->\n        "), body)
  end



  def translate_module_name(name) do
    
      base_name = Nova.String.to_lower((Nova.String.replace_all((Nova.String.pattern(".")), (Nova.String.replacement("_")), name)))
      case base_name do
  "string" -> "nova_string"
  _ -> base_name
end
  end



  def gen_function(ctx, func) do
    
      params = Nova.Array.from_foldable((Nova.Runtime.map((&gen_pattern/1), func.parameters)))
      arity = List.length(func.parameters)
      ctx_with_params = Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, func.parameters)
      params_str = Nova.Runtime.intercalate(", ", params)
      body = if List.null(func.guards) do
        gen_expr(ctx_with_params, func.body)
      else
        gen_guarded_exprs(ctx_with_params, (Nova.Array.from_foldable(func.guards)))
      end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(atom(func.name), "/"), Nova.Runtime.show(arity)), " =\n"), "  fun ("), params_str), ") ->\n"), "    "), body)
  end



  def gen_guarded_exprs(ctx, guards) do
    case Nova.Array.uncons(guards) do
      :nothing -> atom("error_no_matching_guard")
      {:just, %{head: g, tail: rest}} -> gen_one_guarded_expr(ctx, g, rest)
    end
  end



  def gen_one_guarded_expr(ctx, guarded_expr, rest) do
    case List.uncons(guarded_expr.guards) do
      :nothing -> gen_expr(ctx, guarded_expr.body)
      {:just, %{head: clause, tail: more_clauses}} -> case clause do
          {:guard_expr, expr} -> 
              fallback = if (Nova.Array.null(rest) and List.null(more_clauses)) do
                atom("error_no_matching_guard")
              else
                if List.null(more_clauses) do
                  gen_guarded_exprs(ctx, rest)
                else
                  gen_one_guarded_expr(ctx, %{guards: more_clauses, body: guarded_expr.body}, rest)
                end
              end
              Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", gen_expr(ctx, expr)), " of\n"), "        <'true'> when 'true' -> "), gen_expr(ctx, guarded_expr.body)), "\n"), "        <_> when 'true' -> "), fallback), "\n"), "      end")
          {:guard_pat, pat, bind_expr} -> 
              ctx_with_bind = add_locals_from_pattern(pat, ctx)
              fallback = if Nova.Array.null(rest) do
                atom("error_no_matching_guard")
              else
                gen_guarded_exprs(ctx, rest)
              end
              inner_body = if List.null(more_clauses) do
                gen_expr(ctx_with_bind, guarded_expr.body)
              else
                gen_one_guarded_expr(ctx_with_bind, %{guards: more_clauses, body: guarded_expr.body}, rest)
              end
              Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", gen_expr(ctx, bind_expr)), " of\n"), "        <"), gen_pattern(pat)), "> when 'true' -> "), inner_body), "\n"), "        <_> when 'true' -> "), fallback), "\n"), "      end")
        end
    end
  end



  def gen_data_type(_, dt) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("% Data type: ", dt.name), "\n"), "% Constructors: "), Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map(& &1.name, dt.constructors))))))
  end



  def gen_pattern(pat) do
    (gen_pattern_with_counter(pat, 0)).str
  end



  def gen_pattern_with_counter(({:pat_var, name}), n) do
    cond do
      ((Nova.String.take(1, name) == "_")) ->
        %{str: Nova.Runtime.append("_W", Nova.Runtime.show(n)), counter: (n + 1)}
    end
  end

  def gen_pattern_with_counter(({:pat_var, name}), n) do
    %{str: core_var(name), counter: n}
  end

  def gen_pattern_with_counter({:pat_wildcard, n}) do
    %{str: Nova.Runtime.append("_W", Nova.Runtime.show(n)), counter: (n + 1)}
  end

  def gen_pattern_with_counter(({:pat_lit, lit}), n) do
    %{str: gen_literal.(lit), counter: n}
  end

  def gen_pattern_with_counter(({:pat_con, name, pats}), n) do
    
      base_name = case Nova.String.last_index_of((Nova.String.pattern(".")), name) do
        :nothing -> name
        {:just, idx} -> Nova.String.drop(((idx + 1)), name)
      end
      if List.null(pats) do
  %{str: atom((to_snake_case.(base_name))), counter: n}
else
  
    result = gen_pats_with_counter((Nova.Array.from_foldable(pats)), n)
    %{str: Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{", atom((to_snake_case.(base_name)))), ", "), Nova.Runtime.intercalate(", ", result.strs)), "}"), counter: result.counter}
end
  end

  def gen_pattern_with_counter(({:pat_record, fields}), n) do
    
      result = Nova.Runtime.foldl(gen_field_pat, %{strs: [], counter: n}, fields)
      %{str: Nova.Runtime.append(Nova.Runtime.append("~{", Nova.Runtime.intercalate(",", result.strs)), "}~"), counter: result.counter}
  end

  def gen_pattern_with_counter(({:pat_list, pats}), n) do
    
      result = gen_pats_with_counter((Nova.Array.from_foldable(pats)), n)
      %{str: Nova.Runtime.append(Nova.Runtime.append("[", Nova.Runtime.intercalate(", ", result.strs)), "]"), counter: result.counter}
  end

  def gen_pattern_with_counter(({:pat_cons, hd, tl}), n) do
    
      r1 = gen_pattern_with_counter(hd, n)
      r2 = gen_pattern_with_counter(tl, r1.counter)
      %{str: Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("[", r1.str), " | "), r2.str), "]"), counter: r2.counter}
  end

  def gen_pattern_with_counter(({:pat_as, name, pat}), n) do
    
      r = gen_pattern_with_counter(pat, n)
      %{str: Nova.Runtime.append(Nova.Runtime.append(core_var(name), " = "), r.str), counter: r.counter}
  end

  def gen_pattern_with_counter(({:pat_parens, p}), n) do
    gen_pattern_with_counter(p, n)
  end



  def gen_pats_with_counter(pats, n) do
    Nova.Runtime.foldl(step, %{strs: [], counter: n}, pats)
  end



  def gen_expr(_, ({:expr_lit, lit})) do
    gen_literal.(lit)
  end

  def gen_expr(ctx, ({:expr_var, name})) do
    if (name == "otherwise") do
      "'true'"
    else
      case Nova.String.index_of((Nova.String.pattern(".")), name) do
        {:just, idx} -> 
            mod_name = translate_module_name((Nova.String.take(idx, name)))
            func_name = Nova.String.drop(((idx + 1)), name)
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (_Qv0, _Qv1) -> call ", atom(mod_name)), ":"), atom(func_name)), "(_Qv0, _Qv1)")
        :nothing -> if Nova.Set.member(name, ctx.locals) do
            core_var(name)
          else
            case get_prelude_func(name) do
              {:just, info} -> 
                  param_names = (Nova.Array.range(0, ((info.arity - 1))) # Nova.Runtime.map((fn i -> Nova.Runtime.append("_Pf", Nova.Runtime.show(i)) end)))
                  params_str = Nova.Runtime.intercalate(", ", param_names)
                  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (", params_str), ") -> call "), atom(info.mod_)), ":"), atom(info.func)), "("), params_str), ")")
              :nothing -> if Nova.Set.member(name, ctx.module_funcs) do
                  
                    arity = lookup_arity.(name).(ctx)
                    if (arity == 0) do
  Nova.Runtime.append(Nova.Runtime.append("apply ", atom(name)), "/0()")
else
  
    param_names = (Nova.Array.range(0, ((arity - 1))) # Nova.Runtime.map((fn i -> Nova.Runtime.append("_Mf", Nova.Runtime.show(i)) end)))
    params_str = Nova.Runtime.intercalate(", ", param_names)
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (", params_str), ") -> apply "), atom(name)), "/"), Nova.Runtime.show(arity)), "("), params_str), ")")
end
                else
                  case Nova.Map.lookup(name, ctx.imports) do
                    {:just, src_mod} -> 
                        mod_name = translate_module_name(src_mod)
                        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call ", atom(mod_name)), ":"), atom(name)), "()")
                    :nothing -> if is_constructor_name.(name) do
                        atom((to_snake_case.(name)))
                      else
                        core_var(name)
                      end
                  end
                end
            end
          end
      end
    end
  end

  def gen_expr(_ctx, ({:expr_qualified, mod_name, func_name})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call ", atom((translate_module_name(mod_name)))), ":"), atom(func_name)), "()")
  end
end
