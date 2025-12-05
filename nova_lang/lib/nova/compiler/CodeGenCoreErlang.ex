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
    %{var: Nova.Runtime.append("_cor", Nova.Runtime.show(ctx.var_counter)), ctx: %{ctx | var_counter: (ctx.var_counter + 1)}}
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



  def free_vars_in_expr_for(candidates, bound, expr) do
    
      add_pattern_vars = Nova.Runtime.fix2(fn add_pattern_vars -> fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {({:pat_var, n}), s} -> Nova.Set.insert(n, s)
        {:pat_wildcard, s} -> s
        {({:pat_lit, _}), s} -> s
        {({:pat_con, _, pats}), s} -> Nova.Runtime.foldr(add_pattern_vars, s, pats)
        {({:pat_record, fields}), s} -> Nova.Runtime.foldr((fn ({:tuple, _, p}) -> fn acc -> add_pattern_vars.(p).(acc) end end), s, fields)
        {({:pat_list, pats}), s} -> Nova.Runtime.foldr(add_pattern_vars, s, pats)
        {({:pat_cons, hd, tl}), s} -> add_pattern_vars.(hd).((add_pattern_vars.(tl).(s)))
        {({:pat_as, n, p}), s} -> add_pattern_vars.(p).((Nova.Set.insert(n, s)))
        {({:pat_parens, p}), s} -> add_pattern_vars.(p).(s)
      end end end end)
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
      bind_names = Nova.Set.from_foldable((Nova.List.map_maybe((fn b -> get_pattern_var_name(b.pattern) end), binds)))
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
    
      kahn_sort = fn items -> 
        go = Nova.Runtime.fix2(fn go -> fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
          {acc, []} -> acc
          {acc, remaining} -> 
  processed = Nova.Set.from_foldable((Nova.Runtime.map((fn b -> Nova.Runtime.from_maybe("", (get_pattern_var_name(b.pattern))) end), acc)))
  ready = Nova.Array.filter((fn item -> 
    unresolved_deps = Nova.Set.difference(item.deps, processed)
    Nova.Set.is_empty(unresolved_deps) end), remaining)
  not_ready = Nova.Array.filter((fn item -> 
    unresolved_deps = Nova.Set.difference(item.deps, processed)
    not((Nova.Set.is_empty(unresolved_deps))) end), remaining)
  if Nova.Array.null(ready) do
  Nova.Runtime.append(acc, Nova.Runtime.map(& &1.bind, remaining))
else
  go.((Nova.Runtime.append(acc, Nova.Runtime.map(& &1.bind, ready)))).(not_ready)
end
        end end end end)
        go.([]).(items) end
      
  bind_map = Nova.Map.from_foldable((Nova.Array.map_maybe((fn b -> (Nova.Runtime.map((fn n -> {:tuple, n, b} end))).(get_pattern_var_name(b.pattern)) end), binds)))
  bind_names = Nova.Set.from_foldable((Nova.Map.keys(bind_map)))
  deps = Nova.Runtime.map((fn b -> 
    name = Nova.Runtime.from_maybe("", (get_pattern_var_name(b.pattern)))
    free_vars = free_vars_in_expr_for(bind_names, Nova.Set.empty, b.value)
    local_deps = free_vars
    %{name: name, bind: b, deps: local_deps} end), binds)
  sorted = kahn_sort.(deps)
  sorted
  end



  def atom(s) do
    
      escape_atom = fn str -> 
        s1 = Nova.String.replace_all((Nova.String.pattern("\\")), (Nova.String.replacement("\\\\")), str)
        s2 = Nova.String.replace_all((Nova.String.pattern("'")), (Nova.String.replacement("\\'")), s1)
        s2 end
      Nova.Runtime.append(Nova.Runtime.append("'", escape_atom.(s)), "'")
  end



  def core_var(name) do
    
      to_snake = fn s -> Nova.String.replace_all((Nova.String.pattern("'")), (Nova.String.replacement("_")), s) end
      
  first = Nova.String.take(1, name)
  rest = Nova.String.drop(1, name)
  Nova.Runtime.append(Nova.String.to_upper(first), to_snake.(rest))
  end



  def gen_module(m) do
    
      get_func = fn auto_arg0 -> case auto_arg0 do
        ({:decl_function, f}) -> {:just, f}
        _ -> :nothing
      end end
      get_imports = fn auto_arg0 -> case auto_arg0 do
        ({:decl_import, imp}) -> import_items_to_tuples(imp.module_name, (Nova.Array.from_foldable(imp.items)))
        _ -> []
      end end
      gen_decl_non_func = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {ctx, ({:decl_data_type, dt})} -> {:just, (gen_data_type(ctx, dt))}
        {_, _} -> :nothing
      end end end
      
  mod_name = translate_module_name(m.name)
  all_funcs = Nova.Array.map_maybe(get_func, (Nova.Array.from_foldable(m.declarations)))
  all_imports = Nova.Array.concat_map(get_imports, (Nova.Array.from_foldable(m.declarations)))
  import_map = Nova.Map.from_foldable(all_imports)
  grouped = group_functions(all_funcs)
  unique_funcs = Nova.Array.nub_by_eq((fn a -> fn b -> ((a.name == b.name) and (a.arity == b.arity)) end end), (Nova.Runtime.map((fn g -> %{name: g.name, arity: g.arity} end), grouped)))
  exports = Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn f -> Nova.Runtime.append(Nova.Runtime.append(atom(f.name), "/"), Nova.Runtime.show(f.arity)) end), unique_funcs)))
  ctx = %{(empty_ctx(mod_name)) | module_funcs: Nova.Set.from_foldable((Nova.Runtime.map(& &1.name, unique_funcs))), func_arities: unique_funcs, imports: import_map}
  func_defs = Nova.Runtime.intercalate("\n\n", (Nova.Runtime.map((fn auto_p0 -> gen_function_group(ctx, auto_p0) end), grouped)))
  dt_comments = Nova.Runtime.intercalate("\n\n", (Nova.Array.map_maybe((gen_decl_non_func.(ctx)), (Nova.Array.from_foldable(m.declarations)))))
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("module ", atom(mod_name)), " ["), exports), "]\n"), "  attributes []\n"), dt_comments), (if (dt_comments == "") do
  ""
else
  "\n\n"
end)), func_defs), "\nend\n")
  end



  def import_items_to_tuples(mod_name, items) do
    
      to_tuples = fn auto_arg0 -> case auto_arg0 do
        ({:import_value, name}) -> [{:tuple, name, mod_name}]
        ({:import_type, name, _}) -> [{:tuple, name, mod_name}]
      end end
      Nova.Array.concat_map(to_tuples, items)
  end

  # @type function_group :: %{name: string(), arity: int(), clauses: array()(function_declaration())}



  def group_functions(funcs) do
    
      keys = Nova.Array.nub_by_eq((fn a -> fn b -> ((a.name == b.name) and (a.arity == b.arity)) end end), (Nova.Runtime.map((fn f -> %{name: f.name, arity: Nova.List.length(f.parameters)} end), funcs)))
      mk_group = fn k -> %{name: k.name, arity: k.arity, clauses: Nova.Array.filter((fn f -> ((f.name == k.name) and (Nova.List.length(f.parameters) == k.arity)) end), funcs)} end
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
    
      is_complex = fn auto_arg0 -> case auto_arg0 do
        ({:pat_var, _}) -> false
        :pat_wildcard -> false
        _ -> true
      end end
      Nova.List.any(is_complex, pats)
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
      param_names = (Nova.Runtime.map((fn i -> Nova.Runtime.append("_P", Nova.Runtime.show(i)) end))).(Nova.Array.range(0, ((arity - 1))))
      params_str = Nova.Runtime.intercalate(", ", param_names)
      case_clauses = Nova.Runtime.map((fn auto_p0 -> gen_function_clause_as_case(ctx, param_names, auto_p0) end), group.clauses)
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(atom(group.name), "/"), Nova.Runtime.show(arity)), " =\n"), "  fun ("), params_str), ") ->\n"), "    case {"), params_str), "} of\n"), Nova.Runtime.intercalate("\n", case_clauses)), "\n"), "    end")
  end



  def gen_function_clause_as_case(ctx, _param_names, func) do
    
      pats_result = gen_pats_with_counter((Nova.Array.from_foldable(func.parameters)), 0)
      ctx_with_params = Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, func.parameters)
      pat_tuple = Nova.Runtime.append(Nova.Runtime.append("{", Nova.Runtime.intercalate(", ", pats_result.strs)), "}")
      body = if Nova.List.null(func.guards) do
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
      arity = Nova.List.length(func.parameters)
      ctx_with_params = Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, func.parameters)
      params_str = Nova.Runtime.intercalate(", ", params)
      body = if Nova.List.null(func.guards) do
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
    case Nova.List.uncons(guarded_expr.guards) do
      :nothing -> gen_expr(ctx, guarded_expr.body)
      {:just, %{head: clause, tail: more_clauses}} -> case clause do
          {:guard_expr, expr} -> 
              fallback = if (Nova.Array.null(rest) and Nova.List.null(more_clauses)) do
                atom("error_no_matching_guard")
              else
                if Nova.List.null(more_clauses) do
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
              inner_body = if Nova.List.null(more_clauses) do
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

  def gen_pattern_with_counter(:pat_wildcard, n) do
    %{str: Nova.Runtime.append("_W", Nova.Runtime.show(n)), counter: (n + 1)}
  end

  def gen_pattern_with_counter(({:pat_lit, lit}), n) do
    %{str: gen_literal(lit), counter: n}
  end

  def gen_pattern_with_counter(({:pat_con, name, pats}), n) do
    
      base_name = case Nova.String.last_index_of((Nova.String.pattern(".")), name) do
        :nothing -> name
        {:just, idx} -> Nova.String.drop(((idx + 1)), name)
      end
      if Nova.List.null(pats) do
  %{str: atom((to_snake_case(base_name))), counter: n}
else
  
    result = gen_pats_with_counter((Nova.Array.from_foldable(pats)), n)
    %{str: Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{", atom((to_snake_case(base_name)))), ", "), Nova.Runtime.intercalate(", ", result.strs)), "}"), counter: result.counter}
end
  end

  def gen_pattern_with_counter(({:pat_record, fields}), n) do
    
      gen_field_pat = fn acc -> fn ({:tuple, label, pat}) -> 
        r = gen_pattern_with_counter(pat, acc.counter)
        %{strs: Nova.Runtime.append(acc.strs, [Nova.Runtime.append(Nova.Runtime.append(atom((to_snake_case(label))), ":="), r.str)]), counter: r.counter} end end
      
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
    
      step = fn acc -> fn pat -> 
        r = gen_pattern_with_counter(pat, acc.counter)
        %{strs: Nova.Runtime.append(acc.strs, [r.str]), counter: r.counter} end end
      Nova.Runtime.foldl(step, %{strs: [], counter: n}, pats)
  end



  def gen_expr(_, ({:expr_lit, lit})) do
    gen_literal(lit)
  end

  def gen_expr(ctx, ({:expr_var, name})) do
    
      is_constructor_name = fn s -> case Nova.String.take(1, s) do
        c -> ((c >= "A") and (c <= "Z"))
      end end
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
              param_names = (Nova.Runtime.map((fn i -> Nova.Runtime.append("_Pf", Nova.Runtime.show(i)) end))).(Nova.Array.range(0, ((info.arity - 1))))
              params_str = Nova.Runtime.intercalate(", ", param_names)
              Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (", params_str), ") -> call "), atom(info.mod_)), ":"), atom(info.func)), "("), params_str), ")")
          :nothing -> if Nova.Set.member(name, ctx.module_funcs) do
              
                arity = lookup_arity(name, ctx)
                if (arity == 0) do
  Nova.Runtime.append(Nova.Runtime.append("apply ", atom(name)), "/0()")
else
  
    param_names = (Nova.Runtime.map((fn i -> Nova.Runtime.append("_Mf", Nova.Runtime.show(i)) end))).(Nova.Array.range(0, ((arity - 1))))
    params_str = Nova.Runtime.intercalate(", ", param_names)
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (", params_str), ") -> apply "), atom(name)), "/"), Nova.Runtime.show(arity)), "("), params_str), ")")
end
            else
              case Nova.Map.lookup(name, ctx.imports) do
                {:just, src_mod} -> 
                    mod_name = translate_module_name(src_mod)
                    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call ", atom(mod_name)), ":"), atom(name)), "()")
                :nothing -> if is_constructor_name.(name) do
                    atom((to_snake_case(name)))
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

  def gen_expr(ctx, ({:expr_app, f, arg})) do
    
      is_constructor_name = fn s -> case Nova.String.take(1, s) do
        c -> ((c >= "A") and (c <= "Z"))
      end end
      
  %{func: func, args: args} = collect_args((Nova.Compiler.Ast.expr_app(f, arg)))
  case func do
  {:expr_var, name} -> case Nova.String.index_of((Nova.String.pattern(".")), name) do
      {:just, idx} -> 
          mod_name = translate_module_name((Nova.String.take(idx, name)))
          func_name = Nova.String.drop(((idx + 1)), name)
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call ", atom(mod_name)), ":"), atom(func_name)), "("), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args)))), ")")
      :nothing -> if (name == "pure") do
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{", atom("_right")), ", "), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args)))), "}")
        else
          case get_prelude_func(name) do
            {:just, info} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call ", atom(info.mod_)), ":"), atom(info.func)), "("), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args)))), ")")
            :nothing -> if Nova.Set.member(name, ctx.locals) do
                Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("apply ", core_var(name)), "("), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args)))), ")")
              else
                if Nova.Set.member(name, ctx.module_funcs) do
                  
                    declared_arity = lookup_arity(name, ctx)
                    num_args = Nova.Runtime.length(args)
                    if ((declared_arity == 0) and (num_args > 0)) do
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("let <_Fn0> = apply ", atom(name)), "/0()\n"), "      in apply _Fn0("), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args)))), ")")
else
  if (num_args >= declared_arity) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("apply ", atom(name)), "/"), Nova.Runtime.show(declared_arity)), "("), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args)))), ")")
  else
    
      remaining = (declared_arity - num_args)
      param_names = (Nova.Runtime.map((fn i -> Nova.Runtime.append("_Pc", Nova.Runtime.show(i)) end))).(Nova.Array.range(0, ((remaining - 1))))
      params_str = Nova.Runtime.intercalate(", ", param_names)
      all_args = Nova.Runtime.intercalate(", ", (Nova.Runtime.append(Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args), param_names)))
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (", params_str), ") -> apply "), atom(name)), "/"), Nova.Runtime.show(declared_arity)), "("), all_args), ")")
  end
end
                else
                  case Nova.Map.lookup(name, ctx.imports) do
                    {:just, src_mod} -> 
                        mod_name = translate_module_name(src_mod)
                        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call ", atom(mod_name)), ":"), atom(name)), "("), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args)))), ")")
                    :nothing -> if is_constructor_name.(name) do
                        if (Nova.Runtime.length(args) == 0) do
                          atom((to_snake_case(name)))
                        else
                          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("{", atom((to_snake_case(name)))), ", "), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args)))), "}")
                        end
                      else
                        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("apply ", core_var(name)), "("), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args)))), ")")
                      end
                  end
                end
              end
          end
        end
    end
  {:expr_qualified, mod_name, func_name} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call ", atom((translate_module_name(mod_name)))), ":"), atom(func_name)), "("), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args)))), ")")
  _ -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("apply ", gen_expr(ctx, func)), "("), Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), args)))), ")")
end
  end

  def gen_expr(ctx, ({:expr_lambda, pats, body})) do
    case Nova.List.uncons(pats) do
      :nothing -> gen_expr(ctx, body)
      {:just, %{head: pat, tail: rest_pats}} -> if Nova.List.null(rest_pats) do
          if has_complex_pattern_single(pat) do
            
              %{var: var, ctx: ctx_prime} = fresh_var(ctx)
              ctx_with_param = add_locals_from_pattern(pat, ctx_prime)
              pat_result = gen_pattern_with_counter(pat, 0)
              body_code = gen_expr(ctx_with_param, body)
              Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (", var), ") ->\n"), "      case "), var), " of\n"), "        <"), pat_result.str), "> when 'true' -> "), body_code), "\n"), "      end")
          else
            
              pat_result = gen_pattern_with_counter(pat, 0)
              ctx_with_param = add_locals_from_pattern(pat, ctx)
              Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (", pat_result.str), ") ->\n      "), gen_expr(ctx_with_param, body))
          end
        else
          if has_complex_pattern_single(pat) do
            
              %{var: var, ctx: ctx_prime} = fresh_var(ctx)
              ctx_with_param = add_locals_from_pattern(pat, ctx_prime)
              pat_result = gen_pattern_with_counter(pat, 0)
              inner_lambda = gen_expr(ctx_with_param, (Nova.Compiler.Ast.expr_lambda(rest_pats, body)))
              Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (", var), ") ->\n"), "      case "), var), " of\n"), "        <"), pat_result.str), "> when 'true' -> "), inner_lambda), "\n"), "      end")
          else
            
              pat_result = gen_pattern_with_counter(pat, 0)
              ctx_with_param = add_locals_from_pattern(pat, ctx)
              inner_lambda = gen_expr(ctx_with_param, (Nova.Compiler.Ast.expr_lambda(rest_pats, body)))
              Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (", pat_result.str), ") ->\n      "), inner_lambda)
          end
        end
    end
  end

  def gen_expr(ctx, ({:expr_let, binds, body})) do
    gen_let_binds_with_body(ctx, (Nova.Array.from_foldable(binds)), body)
  end

  def gen_expr(ctx, ({:expr_if, cond_, then_e, else_e})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", gen_expr(ctx, cond_)), " of\n"), "      <"), atom("true")), "> when 'true' -> "), gen_expr(ctx, then_e)), "\n"), "      <"), atom("false")), "> when 'true' -> "), gen_expr(ctx, else_e)), "\n"), "    end")
  end

  def gen_expr(ctx, ({:expr_case, scrutinee, clauses})) do
    
      scrut_expr = gen_expr(ctx, scrutinee)
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", scrut_expr), " of\n"), gen_case_clauses_with_fallback(ctx, scrut_expr, clauses)), "\n"), "    end")
  end

  def gen_expr(ctx, ({:expr_bin_op, ":", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("[", gen_expr(ctx, l)), " | "), gen_expr(ctx, r)), "]")
  end

  def gen_expr(ctx, ({:expr_bin_op, "<>", l, r})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call 'erlang':'++'(", gen_expr(ctx, l)), ", "), gen_expr(ctx, r)), ")")
  end

  def gen_expr(ctx, ({:expr_bin_op, op, l, r})) do
    case Nova.String.index_of((Nova.String.pattern(".")), op) do
      {:just, idx} -> 
          mod_name = translate_module_name((Nova.String.take(idx, op)))
          func_name = Nova.String.drop(((idx + 1)), op)
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call ", atom(mod_name)), ":"), atom(func_name)), "("), gen_expr(ctx, l)), ", "), gen_expr(ctx, r)), ")")
      :nothing -> case op do
          "&&" -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", gen_expr(ctx, l)), " of <'true'> when 'true' -> "), gen_expr(ctx, r)), " <_> when 'true' -> 'false' end")
          "||" -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", gen_expr(ctx, l)), " of <'true'> when 'true' -> 'true'"), " <_> when 'true' -> "), gen_expr(ctx, r)), " end")
          _ -> 
              erl_op = translate_op(op)
              Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call 'erlang':'", erl_op), "'("), gen_expr(ctx, l)), ", "), gen_expr(ctx, r)), ")")
        end
    end
  end

  def gen_expr(ctx, ({:expr_unary_op, "-", e})) do
    Nova.Runtime.append(Nova.Runtime.append("call 'erlang':'-'(", gen_expr(ctx, e)), ")")
  end

  def gen_expr(ctx, ({:expr_unary_op, "!", e})) do
    Nova.Runtime.append(Nova.Runtime.append("call 'erlang':'not'(", gen_expr(ctx, e)), ")")
  end

  def gen_expr(ctx, ({:expr_unary_op, _, e})) do
    gen_expr(ctx, e)
  end

  def gen_expr(ctx, ({:expr_list, elems})) do
    gen_core_list(ctx, elems)
  end

  def gen_expr(ctx, ({:expr_tuple, elems})) do
    Nova.Runtime.append(Nova.Runtime.append("{", Nova.Runtime.intercalate(", ", (Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), elems)))))), "}")
  end

  def gen_expr(ctx, ({:expr_record, fields})) do
    
      gen_field = fn ({:tuple, label, expr}) -> Nova.Runtime.append(Nova.Runtime.append(atom((to_snake_case(label))), "=>"), gen_expr(ctx, expr)) end
      Nova.Runtime.append(Nova.Runtime.append("~{", Nova.Runtime.intercalate(",", (Nova.Array.from_foldable((Nova.Runtime.map(gen_field, fields)))))), "}~")
  end

  def gen_expr(ctx, ({:expr_record_access, expr, field})) do
    case expr do
      {:expr_var, "_"} -> Nova.Runtime.append(Nova.Runtime.append("fun (_Ra) -> call 'maps':'get'(", atom((to_snake_case(field)))), ", _Ra)")
      _ -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call 'maps':'get'(", atom((to_snake_case(field)))), ", "), gen_expr(ctx, expr)), ")")
    end
  end

  def gen_expr(ctx, ({:expr_record_update, expr, fields})) do
    
      gen_field_update = fn ({:tuple, label, val}) -> Nova.Runtime.append(Nova.Runtime.append(atom((to_snake_case(label))), "=>"), gen_expr(ctx, val)) end
      
  updates = Nova.Runtime.intercalate(",", (Nova.Array.from_foldable((Nova.Runtime.map(gen_field_update, fields)))))
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("call 'maps':'merge'(", gen_expr(ctx, expr)), ", ~{"), updates), "}~)")
  end

  def gen_expr(ctx, ({:expr_parens, e})) do
    gen_expr(ctx, e)
  end

  def gen_expr(ctx, ({:expr_do, stmts})) do
    gen_do_stmts(ctx, (Nova.Array.from_foldable(stmts)))
  end

  def gen_expr(ctx, ({:expr_typed, e, _})) do
    gen_expr(ctx, e)
  end

  def gen_expr(_ctx, ({:expr_section, op})) do
    Nova.Runtime.append(Nova.Runtime.append("fun (X, Y) -> call 'erlang':'", translate_op(op)), "'(X, Y)")
  end

  def gen_expr(ctx, ({:expr_section_left, e, op})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (X) -> call 'erlang':'", translate_op(op)), "'("), gen_expr(ctx, e)), ", X)")
  end

  def gen_expr(ctx, ({:expr_section_right, op, e})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (X) -> call 'erlang':'", translate_op(op)), "'(X, "), gen_expr(ctx, e)), ")")
  end



  def gen_expr_letrec(ctx, expr) do
    case expr do
      {:expr_lambda, pats, body} -> if has_complex_pattern(pats) do
          
            arity = Nova.List.length(pats)
            pat_result = gen_pats_with_counter((Nova.Array.from_foldable(pats)), 0)
            ctx_with_params = Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, pats)
            param_names = (Nova.Runtime.map((fn i -> Nova.Runtime.append("_L", Nova.Runtime.show(i)) end))).(Nova.Array.range(0, ((arity - 1))))
            pat_tuple = Nova.Runtime.append(Nova.Runtime.append("{", Nova.Runtime.intercalate(", ", pat_result.strs)), "}")
            body_code = gen_expr(ctx_with_params, body)
            params_str = Nova.Runtime.intercalate(", ", param_names)
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (", params_str), ") ->\n"), "      case {"), params_str), "} of\n"), "        <"), pat_tuple), "> when 'true' -> "), body_code), "\n"), "      end")
        else
          
            pat_result = gen_pats_with_counter((Nova.Array.from_foldable(pats)), 0)
            ctx_with_params = Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx, pats)
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("fun (", Nova.Runtime.intercalate(", ", pat_result.strs)), ") ->\n      "), gen_expr(ctx_with_params, body))
        end
      _ -> gen_expr(ctx, expr)
    end
  end



  def expr_contains_var(name, expr) do
    case expr do
      {:expr_var, v} -> (v == name)
      {:expr_qualified, _, _} -> false
      {:expr_lit, _} -> false
      {:expr_app, f, a} -> (expr_contains_var(name, f) or expr_contains_var(name, a))
      {:expr_lambda, _, body} -> expr_contains_var(name, body)
      {:expr_let, binds, body} -> (Nova.List.any((fn b -> expr_contains_var(name, b.value) end), binds) or expr_contains_var(name, body))
      {:expr_if, c, t, e} -> ((expr_contains_var(name, c) or expr_contains_var(name, t)) or expr_contains_var(name, e))
      {:expr_case, scrut, clauses} -> (expr_contains_var(name, scrut) or Nova.List.any((fn cl -> expr_contains_var(name, cl.body) end), clauses))
      {:expr_do, stmts} -> Nova.List.any((fn auto_p0 -> do_stmt_contains_var(name, auto_p0) end), stmts)
      {:expr_bin_op, _, l, r} -> (expr_contains_var(name, l) or expr_contains_var(name, r))
      {:expr_unary_op, _, e} -> expr_contains_var(name, e)
      {:expr_list, elems} -> Nova.List.any((fn auto_p0 -> expr_contains_var(name, auto_p0) end), elems)
      {:expr_tuple, elems} -> Nova.List.any((fn auto_p0 -> expr_contains_var(name, auto_p0) end), elems)
      {:expr_record, fields} -> Nova.List.any((fn ({:tuple, _, e}) -> expr_contains_var(name, e) end), fields)
      {:expr_record_access, e, _} -> expr_contains_var(name, e)
      {:expr_record_update, e, updates} -> (expr_contains_var(name, e) or Nova.List.any((fn ({:tuple, _, e_prime}) -> expr_contains_var(name, e_prime) end), updates))
      {:expr_typed, e, _} -> expr_contains_var(name, e)
      {:expr_parens, e} -> expr_contains_var(name, e)
      {:expr_section, _} -> false
      {:expr_section_left, e, _} -> expr_contains_var(name, e)
      {:expr_section_right, _, e} -> expr_contains_var(name, e)
    end
  end



  def do_stmt_contains_var(name, stmt) do
    case stmt do
      {:do_let, binds} -> Nova.List.any((fn b -> expr_contains_var(name, b.value) end), binds)
      {:do_bind, _, e} -> expr_contains_var(name, e)
      {:do_expr, e} -> expr_contains_var(name, e)
    end
  end



  def get_pattern_var_name(({:pat_var, n})) do
    {:just, n}
  end

  def get_pattern_var_name(_) do
    :nothing
  end



  def is_recursive_bind(bind) do
    case get_pattern_var_name(bind.pattern) do
      {:just, name} -> expr_contains_var(name, bind.value)
      :nothing -> false
    end
  end



  def get_lambda_arity(({:expr_lambda, pats, _})) do
    Nova.List.length(pats)
  end

  def get_lambda_arity(({:expr_parens, e})) do
    get_lambda_arity(e)
  end

  def get_lambda_arity(_) do
    0
  end



  def is_simple_pattern(({:pat_var, _})) do
    true
  end

  def is_simple_pattern(:pat_wildcard) do
    true
  end

  def is_simple_pattern(_) do
    false
  end



  def gen_let_binds_with_body(ctx, binds, body) do
    
      add_value_bind_to_ctx = fn bind -> fn ctx_prime -> 
        bind_name = get_pattern_var_name(bind.pattern)
        case bind_name do
  {:just, n} -> %{ctx_prime | locals: Nova.Set.insert(n, ctx_prime.locals)}
  :nothing -> add_locals_from_pattern(bind.pattern, ctx_prime)
end end end
      gen_letrec_def = fn ctx_prime -> fn bind -> 
        bind_name = Nova.Runtime.from_maybe("_anon", (get_pattern_var_name(bind.pattern)))
        arity = get_lambda_arity(bind.value)
        val = gen_expr_letrec(ctx_prime, bind.value)
        Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(atom(bind_name), "/"), Nova.Runtime.show(arity)), " = "), val) end end
      gen_letrec_clause_as_case = Nova.Runtime.fix3(fn gen_letrec_clause_as_case -> fn ctx_prime -> fn _param_names -> fn bind -> case bind.value do
        {:expr_lambda, pats, body} -> 
            pats_result = gen_pats_with_counter((Nova.Array.from_foldable(pats)), 0)
            ctx_with_params = Nova.Runtime.foldr((&add_locals_from_pattern/2), ctx_prime, pats)
            pat_tuple = Nova.Runtime.append(Nova.Runtime.append("{", Nova.Runtime.intercalate(", ", pats_result.strs)), "}")
            body_str = gen_expr(ctx_with_params, body)
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("        <", pat_tuple), "> when 'true' -> "), body_str)
        {:expr_parens, e} -> gen_letrec_clause_as_case.(ctx_prime).(_param_names).((%{bind | value: e}))
        _ -> Nova.Runtime.append("        <_W0> when 'true' -> ", gen_expr(ctx_prime, bind.value))
      end  end end end end)
      gen_letrec_def_grouped = fn ctx_prime -> fn group -> case group.binds do
        [] -> ""
        [single] -> gen_letrec_def.(ctx_prime).(single)
        multiple -> 
            param_names = (Nova.Runtime.map((fn i -> Nova.Runtime.append("_L", Nova.Runtime.show(i)) end))).(Nova.Array.range(0, ((group.arity - 1))))
            params_str = Nova.Runtime.intercalate(", ", param_names)
            case_clauses = Nova.Runtime.map((gen_letrec_clause_as_case.(ctx_prime).(param_names)), multiple)
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(atom(group.name), "/"), Nova.Runtime.show(group.arity)), " = fun ("), params_str), ") ->\n"), "      case {"), params_str), "} of\n"), Nova.Runtime.intercalate("\n", case_clauses)), "\n"), "      end")
      end end end
      
  is_lambda_bind = fn b -> (get_lambda_arity(b.value) > 0) end
  func_binds = Nova.Array.filter(is_lambda_bind, binds)
  value_binds = Nova.Array.filter((fn auto_c -> ((&Kernel.not/1)).((is_lambda_bind).(auto_c)) end), binds)
  letrec_funcs = Nova.Array.map_maybe((fn b -> (Nova.Runtime.map((fn n -> %{name: n, arity: get_lambda_arity(b.value)} end))).(get_pattern_var_name(b.pattern)) end), func_binds)
  value_names = Nova.Set.from_foldable((Nova.Array.map_maybe((fn b -> get_pattern_var_name(b.pattern) end), value_binds)))
  func_names = Nova.Set.from_foldable((Nova.Runtime.map(& &1.name, letrec_funcs)))
  ctx_with_funcs = %{ctx | module_funcs: Nova.Runtime.foldr((fn f -> fn s -> Nova.Set.insert(f.name, s) end end), ctx.module_funcs, letrec_funcs), func_arities: Nova.Runtime.append(ctx.func_arities, letrec_funcs)}
  ctx_with_all_binds = Nova.Runtime.foldr(add_value_bind_to_ctx, ctx_with_funcs, value_binds)
  uses_func = fn b -> not((Nova.Set.is_empty((Nova.Set.intersection((free_vars_in_expr_for(func_names, Nova.Set.empty, b.value)), func_names))))) end
  independent_value_binds = Nova.Array.filter((fn auto_c -> ((&Kernel.not/1)).((uses_func).(auto_c)) end), value_binds)
  dependent_value_binds = Nova.Array.filter(uses_func, value_binds)
  dependent_value_names = Nova.Set.from_foldable((Nova.Array.map_maybe((fn b -> get_pattern_var_name(b.pattern) end), dependent_value_binds)))
  func_uses_dep_value = fn b -> not((Nova.Set.is_empty((Nova.Set.intersection((free_vars_in_expr_for(dependent_value_names, Nova.Set.empty, b.value)), dependent_value_names))))) end
  func_group1 = Nova.Array.filter((fn auto_c -> ((&Kernel.not/1)).((func_uses_dep_value).(auto_c)) end), func_binds)
  func_group2 = Nova.Array.filter(func_uses_dep_value, func_binds)
  func_group2_names = Nova.Set.from_foldable((Nova.Array.map_maybe((fn b -> get_pattern_var_name(b.pattern) end), func_group2)))
  value_uses_func_group2 = fn b -> not((Nova.Set.is_empty((Nova.Set.intersection((free_vars_in_expr_for(func_group2_names, Nova.Set.empty, b.value)), func_group2_names))))) end
  dep_values_group1 = Nova.Array.filter((fn auto_c -> ((&Kernel.not/1)).((value_uses_func_group2).(auto_c)) end), dependent_value_binds)
  dep_values_group2 = Nova.Array.filter(value_uses_func_group2, dependent_value_binds)
  if Nova.Array.null(func_binds) do
  gen_value_binds_with_body(ctx_with_all_binds, value_binds, body)
else
  
    group_by_name = Nova.Runtime.fix(fn group_by_name -> fn auto_arg0 -> case auto_arg0 do
      binds_ -> 
  names = Nova.Array.nub((Nova.Array.map_maybe((fn b -> get_pattern_var_name(b.pattern) end), binds_)))
  mk_group = fn n -> 
    matching = Nova.Array.filter((fn b -> (get_pattern_var_name(b.pattern) == {:just, n}) end), binds_)
    arity = case Nova.Array.head(matching) do
      {:just, b} -> get_lambda_arity(b.value)
      :nothing -> 0
    end
    %{name: n, binds: matching, arity: arity} end
  Nova.Runtime.map(mk_group, names)
    end end end)
    final_body = gen_expr(ctx_with_all_binds, body)
    gen_letrec = Nova.Runtime.fix2(fn gen_letrec -> fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
      {fs, body_str} -> if Nova.Array.null(fs) do
  body_str
else
  
    grouped = group_by_name.(fs)
    defs = Nova.Runtime.intercalate("\n       ", (Nova.Runtime.map((gen_letrec_def_grouped.(ctx_with_all_binds)), grouped)))
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("letrec ", defs), "\n      in "), body_str)
end
    end end end end)
    after_dep_values2 = if Nova.Array.null(dep_values_group2) do
      final_body
    else
      gen_value_binds_with_body_str_sorted(ctx_with_all_binds, (topo_sort_binds(dep_values_group2)), final_body)
    end
    after_letrec2 = gen_letrec.(func_group2).(after_dep_values2)
    after_dep_values1 = if Nova.Array.null(dep_values_group1) do
      after_letrec2
    else
      gen_value_binds_with_body_str_sorted(ctx_with_all_binds, (topo_sort_binds(dep_values_group1)), after_letrec2)
    end
    after_letrec1 = gen_letrec.(func_group1).(after_dep_values1)
    result = if Nova.Array.null(independent_value_binds) do
      after_letrec1
    else
      gen_value_binds_with_body_str_sorted(ctx_with_all_binds, (topo_sort_binds(independent_value_binds)), after_letrec1)
    end
    result
end
  end



  def gen_value_binds_with_body(ctx, binds, body) do
    
      sorted_binds = topo_sort_binds(binds)
      gen_value_binds_with_body_str_sorted(ctx, sorted_binds, (gen_expr(ctx, body)))
  end



  def gen_value_binds_with_body_str(ctx, binds, body_str) do
    
      sorted_binds = topo_sort_binds(binds)
      gen_value_binds_with_body_str_sorted(ctx, sorted_binds, body_str)
  end



  def gen_value_binds_with_body_str_sorted(ctx, binds, body_str) do
    case Nova.Array.uncons(binds) do
      :nothing -> body_str
      {:just, %{head: bind, tail: rest}} -> 
          pat = gen_pattern(bind.pattern)
          val = gen_expr(ctx, bind.value)
          tmp_var = Nova.Runtime.append("_Let", Nova.Runtime.show(ctx.var_counter))
          next_ctx = %{ctx | var_counter: (ctx.var_counter + 1)}
          continuation = gen_value_binds_with_body_str_sorted(ctx, rest, body_str)
          continuation_complex = gen_value_binds_with_body_str_sorted(next_ctx, rest, body_str)
          if is_simple_pattern(bind.pattern) do
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("let <", pat), "> = "), val), "\n      in "), continuation)
else
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("let <", tmp_var), "> = "), val), "\n      in case "), tmp_var), " of\n        <"), pat), "> when 'true' -> "), continuation_complex), "\n      end")
end
    end
  end



  def gen_let_binds(ctx, binds) do
    
      gen_bind = fn bind -> fn acc -> 
        pat = gen_pattern(bind.pattern)
        bind_names = get_pattern_var_name(bind.pattern)
        is_rec = is_recursive_bind(bind)
        arity = get_lambda_arity(bind.value)
        ctx_for_val = case bind_names do
          {:just, n} when is_rec -> %{acc.snd | module_funcs: Nova.Set.insert(n, acc.snd.module_funcs), func_arities: Nova.Runtime.append(acc.snd.func_arities, [%{name: n, arity: arity}])}
          _ -> acc.snd
        end
        func_name = case bind_names do
          {:just, n} -> to_snake_case(n)
          :nothing -> "_anon"
        end
        val = gen_expr(ctx_for_val, bind.value)
        new_ctx = if is_rec do
          ctx_for_val
        else
          add_locals_from_pattern(bind.pattern, acc.snd)
        end
        if is_rec do
  %{fst: Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("letrec ", atom(func_name)), "/"), Nova.Runtime.show(arity)), " = "), val), "\n      in "), acc.fst), snd: new_ctx}
else
  %{fst: Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("let <", pat), "> = "), val), "\n      in "), acc.fst), snd: new_ctx}
end end end
      Nova.Runtime.foldr(gen_bind, %{fst: "", snd: ctx}, binds)
  end



  def is_wildcard_pattern(({:pat_var, _})) do
    true
  end

  def is_wildcard_pattern(:pat_wildcard) do
    true
  end

  def is_wildcard_pattern(_) do
    false
  end



  def gen_case_clauses_with_fallback(ctx, scrut_expr, clauses) do
    case Nova.List.uncons(clauses) do
      :nothing -> "      <_> when 'true' -> primop 'match_fail'({'case_clause', 'no_clause'})"
      {:just, %{head: clause, tail: rest}} -> if (is_wildcard_pattern(clause.pattern) and Nova.Runtime.is_just(clause.guard)) do
          gen_guarded_wildcard_clause(ctx, scrut_expr, clause, rest)
        else
          
            %{same_pattern: same_pattern, different: different} = split_same_pattern_clauses(clause.pattern, rest)
            ctx_with_vars = add_locals_from_pattern(clause.pattern, ctx)
            fallback = if (Nova.List.null(same_pattern) and ((Nova.List.null(different) or Nova.Runtime.is_nothing(clause.guard)))) do
              :nothing
            else
              {:just, (gen_fallback_for_same_pattern(ctx_with_vars, scrut_expr, same_pattern, different))}
            end
            Nova.Runtime.append(gen_case_clause(ctx, clause, fallback), (if Nova.List.null(different) do
  ""
else
  Nova.Runtime.append("\n", gen_case_clauses_with_fallback(ctx, scrut_expr, different))
end))
        end
    end
  end



  def split_same_pattern_clauses(pat, clauses) do
    
      is_same_guarded = fn c -> ((gen_pattern(c.pattern) == gen_pattern(pat)) and Nova.Runtime.is_just(c.guard)) end
      same_pattern = Nova.List.take_while(is_same_guarded, clauses)
      different = Nova.List.drop((Nova.List.length(same_pattern)), clauses)
      %{same_pattern: same_pattern, different: different}
  end



  def gen_fallback_for_same_pattern(ctx, scrut_expr, same_pattern, different) do
    case Nova.List.uncons(same_pattern) do
      :nothing -> gen_fallback_for_different(ctx, scrut_expr, different)
      {:just, %{head: clause, tail: rest}} -> 
          guard_expr = case clause.guard do
            {:just, g} -> gen_expr(ctx, g)
            :nothing -> "'true'"
          end
          body = gen_expr(ctx, clause.body)
          fallback = gen_fallback_for_same_pattern(ctx, scrut_expr, rest, different)
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", guard_expr), " of\n"), "            <'true'> when 'true' -> "), body), "\n"), "            <_> when 'true' -> "), fallback), "\n"), "          end")
    end
  end



  def gen_fallback_for_different(ctx, scrut_expr, clauses) do
    case Nova.List.uncons(clauses) do
      :nothing -> "primop 'match_fail'({'case_clause', 'no_match'})"
      {:just, %{head: clause, tail: rest}} -> if is_wildcard_pattern(clause.pattern) do
          case clause.guard do
            {:just, g} -> 
                body = gen_expr(ctx, clause.body)
                fallback = gen_fallback_for_different(ctx, scrut_expr, rest)
                Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", gen_expr(ctx, g)), " of\n"), "            <'true'> when 'true' -> "), body), "\n"), "            <_> when 'true' -> "), fallback), "\n"), "          end")
            :nothing -> gen_expr(ctx, clause.body)
          end
        else
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", scrut_expr), " of\n"), gen_case_clauses_with_fallback(ctx, scrut_expr, clauses)), "\n"), "          end")
        end
    end
  end



  def gen_guarded_wildcard_clause(ctx, scrut_expr, clause, rest) do
    
      pat = gen_pattern(clause.pattern)
      ctx_with_vars = add_locals_from_pattern(clause.pattern, ctx)
      guard_expr = case clause.guard do
        {:just, g} -> gen_expr(ctx_with_vars, g)
        :nothing -> "'true'"
      end
      body = gen_expr(ctx_with_vars, clause.body)
      fallback = if Nova.List.null(rest) do
        "primop 'match_fail'({'case_clause', 'guard_failed'})"
      else
        gen_fallback_for_different(ctx_with_vars, scrut_expr, rest)
      end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("      <", pat), "> when 'true' ->\n"), "        case "), guard_expr), " of\n"), "          <'true'> when 'true' -> "), body), "\n"), "          <_> when 'true' -> "), fallback), "\n"), "        end")
  end



  def gen_case_clause(ctx, clause, fallback) do
    
      pat = gen_pattern(clause.pattern)
      ctx_with_vars = add_locals_from_pattern(clause.pattern, ctx)
      body = case clause.guard do
        :nothing -> gen_expr(ctx_with_vars, clause.body)
        {:just, g} -> 
            fallback_code = case fallback do
              {:just, fb} -> fb
              :nothing -> Nova.Runtime.append(Nova.Runtime.append("primop 'match_fail'({'case_clause', ", gen_expr(ctx_with_vars, g)), "})")
            end
            Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", gen_expr(ctx_with_vars, g)), " of\n"), "          <'true'> when 'true' -> "), gen_expr(ctx_with_vars, clause.body)), "\n"), "          <_> when 'true' -> "), fallback_code), "\n"), "        end")
      end
      Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("      <", pat), "> when 'true' ->\n        "), body)
  end



  def gen_do_stmts(ctx, stmts) do
    case Nova.Array.uncons(stmts) do
      :nothing -> atom("unit")
      {:just, %{head: {:do_expr, e}, tail: []}} -> gen_expr(ctx, e)
      {:just, %{head: {:do_expr, e}, tail: rest}} -> 
          %{var: var, ctx: ctx_prime} = fresh_var(ctx)
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("let <", var), "> = "), gen_expr(ctx, e)), "\n      in "), gen_do_stmts(ctx_prime, rest))
      {:just, %{head: {:do_bind, pat, e}, tail: rest}} -> 
          ctx_with_bind = add_locals_from_pattern(pat, ctx)
          bind_expr = gen_expr(ctx, e)
          pat_code = gen_pattern(pat)
          rest_code = gen_do_stmts(ctx_with_bind, rest)
          Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("case ", bind_expr), " of\n"), "      <'_nothing'> when 'true' ->\n        '_nothing'\n"), "      <{'_left', _Err}> when 'true' ->\n        {'_left', _Err}\n"), "      <{'_just', "), pat_code), "}> when 'true' ->\n        "), rest_code), "\n"), "      <{'_right', "), pat_code), "}> when 'true' ->\n        "), rest_code), "\n      end")
      {:just, %{head: {:do_let, binds}, tail: rest}} -> gen_do_let_with_body(ctx, (Nova.Array.from_foldable(binds)), (Nova.Array.from_foldable(rest)))
    end
  end



  def gen_do_let_with_body(ctx, binds, rest) do
    case Nova.Array.uncons(binds) do
      :nothing -> gen_do_stmts(ctx, rest)
      {:just, %{head: bind, tail: more_binds}} -> 
          pat = gen_pattern(bind.pattern)
          bind_names = get_pattern_var_name(bind.pattern)
          is_rec = is_recursive_bind(bind)
          arity = get_lambda_arity(bind.value)
          ctx_for_val = case bind_names do
            {:just, n} when is_rec -> %{ctx | module_funcs: Nova.Set.insert(n, ctx.module_funcs), func_arities: Nova.Runtime.append(ctx.func_arities, [%{name: n, arity: arity}])}
            _ -> ctx
          end
          func_name = case bind_names do
            {:just, n} -> to_snake_case(n)
            :nothing -> "_anon"
          end
          val = gen_expr(ctx_for_val, bind.value)
          new_ctx = if is_rec do
            ctx_for_val
          else
            add_locals_from_pattern(bind.pattern, ctx)
          end
          tmp_var = Nova.Runtime.append("_DLet", Nova.Runtime.show(new_ctx.var_counter))
          next_ctx = %{new_ctx | var_counter: (new_ctx.var_counter + 1)}
          continuation = gen_do_let_with_body(new_ctx, more_binds, rest)
          continuation_complex = gen_do_let_with_body(next_ctx, more_binds, rest)
          if is_rec do
  Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("letrec ", atom(func_name)), "/"), Nova.Runtime.show(arity)), " = "), val), "\n      in "), continuation)
else
  if is_simple_pattern(bind.pattern) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("let <", pat), "> = "), val), "\n      in "), continuation)
  else
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("let <", tmp_var), "> = "), val), "\n      in case "), tmp_var), " of\n        <"), pat), "> when 'true' -> "), continuation_complex), "\n      end")
  end
end
    end
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
    Nova.Runtime.append("$", escape_char(c))
  end

  def gen_literal(({:lit_bool, true})) do
    atom("true")
  end

  def gen_literal(({:lit_bool, false})) do
    atom("false")
  end



  def escape_string(s) do
    
      s1 = Nova.String.replace_all((Nova.String.pattern("\\")), (Nova.String.replacement("\\\\")), s)
      s2 = Nova.String.replace_all((Nova.String.pattern("\"")), (Nova.String.replacement("\\\"")), s1)
      s3 = Nova.String.replace_all((Nova.String.pattern("\n")), (Nova.String.replacement("\\n")), s2)
      s4 = Nova.String.replace_all((Nova.String.pattern("\t")), (Nova.String.replacement("\\t")), s3)
      s5 = Nova.String.replace_all((Nova.String.pattern("")), (Nova.String.replacement("\\r")), s4)
      s5
  end



  def escape_char(c) do
    case c do
      ?\n -> "\\n"
      ?\r -> "\\r"
      ?\t -> "\\t"
      ?\\ -> "\\\\"
      _ -> Nova.String.singleton(c)
    end
  end



  def translate_op("+") do
    "+"
  end

  def translate_op("-") do
    "-"
  end

  def translate_op("*") do
    "*"
  end

  def translate_op("/") do
    "div"
  end

  def translate_op("==") do
    "=:="
  end

  def translate_op("/=") do
    "=/="
  end

  def translate_op("<") do
    "<"
  end

  def translate_op(">") do
    ">"
  end

  def translate_op("<=") do
    "=<"
  end

  def translate_op(">=") do
    ">="
  end

  def translate_op("&&") do
    "andalso"
  end

  def translate_op("||") do
    "orelse"
  end

  def translate_op(op) do
    op
  end



  def to_snake_case(s) do
    
      is_upper = fn c -> ((c >= ?A) and (c <= ?Z)) end
      to_lower = fn c -> Nova.Runtime.from_maybe(c, (Nova.String.char_at(0, (Nova.String.to_lower((Nova.String.singleton(c))))))) end
      convert_char = fn c -> if is_upper.(c) do
        [?_, to_lower.(c)]
      else
        [c]
      end end
      
  chars = Nova.String.to_char_array(s)
  Nova.String.from_char_array((Nova.Array.concat_map(convert_char, chars)))
  end



  def gen_core_list(ctx, elems) do
    if (Nova.List.length(elems) <= 50) do
      Nova.Runtime.append(Nova.Runtime.append("[", Nova.Runtime.intercalate(", ", (Nova.Runtime.map((fn auto_p0 -> gen_expr(ctx, auto_p0) end), (Nova.List.to_unfoldable(elems)))))), "]")
    else
      case Nova.List.uncons(elems) do
        :nothing -> "[]"
        {:just, %{head: h, tail: []}} -> Nova.Runtime.append(Nova.Runtime.append("[", gen_expr(ctx, h)), "]")
        {:just, %{head: h, tail: t}} -> Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append("[", gen_expr(ctx, h)), "|"), gen_core_list(ctx, t)), "]")
      end
    end
  end



  def collect_args(expr) do
    
      go = Nova.Runtime.fix2(fn go -> fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {({:expr_app, f, a}), acc} -> go.(f).(([a | acc]))
        {f, acc} -> %{func: f, args: acc}
      end end end end)
      go.(expr).([])
  end



  def lookup_arity(name, ctx) do
    case Nova.Array.find((fn f -> (f.name == name) end), ctx.func_arities) do
      {:just, f} -> f.arity
      :nothing -> 0
    end
  end
end
