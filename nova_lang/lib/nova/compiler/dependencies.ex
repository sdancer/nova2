defmodule Nova.Compiler.Dependencies do
  # import Prelude

  # import Data.Array

  # import Data.Foldable

  # import Data.Map

  # import Data.Map

  # import Data.Maybe

  # import Data.Set

  # import Data.Set

  # import Data.Tuple

  # import Nova.Compiler.Ast

  def get_dependencies(decl) do
    case decl do
      {:decl_function, f} -> 
          body_deps = get_expr_deps(f.body)
          guard_deps = Nova.Runtime.foldl(fn acc -> fn g -> Nova.Set.union(acc, get_guarded_expr_deps(g)) end end, Nova.Set.empty, f.guards)
          param_names = Nova.Runtime.foldl(fn acc -> fn p -> Nova.Set.union(acc, get_bound_names(p)) end end, Nova.Set.empty, f.parameters)
          Nova.Set.difference(Nova.Set.union(body_deps, guard_deps), param_names)
      {:decl_data_type, d} -> Nova.Runtime.foldl(fn acc -> fn c -> Nova.Set.union(acc, get_constructor_deps(c)) end end, Nova.Set.empty, d.constructors)
      {:decl_type_alias, a} -> get_type_expr_deps(a.ty)
      {:decl_type_class, c} -> Nova.Runtime.foldl(fn acc -> fn m -> Nova.Set.union(acc, get_type_expr_deps(m.ty)) end end, Nova.Set.empty, c.methods)
      {:decl_type_class_instance, i} -> 
          ty_deps = get_type_expr_deps(i.ty)
          method_deps = Nova.Runtime.foldl(fn acc -> fn m -> Nova.Set.union(acc, get_expr_deps(m.body)) end end, Nova.Set.empty, i.methods)
          Nova.Set.union(ty_deps, method_deps)
      {:decl_foreign_import, f} -> get_type_expr_deps(f.type_signature)
      {:decl_type_sig, s} -> get_type_expr_deps(s.ty)
      {:decl_type, t} -> get_type_expr_deps(t.type_signature)
      {:decl_module, m} -> Nova.Runtime.foldl(fn acc -> fn d -> Nova.Set.union(acc, get_dependencies(d)) end end, Nova.Set.empty, m.declarations)
      {:decl_import, _} -> Nova.Set.empty
      {:decl_newtype, n} -> get_type_expr_deps(n.wrapped_type)
      {:decl_infix, _} -> Nova.Set.empty
    end
  end

  def get_constructor_deps(c) do
    Nova.Runtime.foldl(fn acc -> fn f -> Nova.Set.union(acc, get_type_expr_deps(f.ty)) end end, Nova.Set.empty, c.fields)
  end

  def get_type_expr_deps(ty) do
    case ty do
      {:ty_expr_con, name} -> Nova.Set.singleton(name)
      {:ty_expr_var, _} -> Nova.Set.empty
      {:ty_expr_app, t1, t2} -> Nova.Set.union(get_type_expr_deps(t1), get_type_expr_deps(t2))
      {:ty_expr_arrow, t1, t2} -> Nova.Set.union(get_type_expr_deps(t1), get_type_expr_deps(t2))
      {:ty_expr_record, fields, _} -> Nova.Runtime.foldl(fn acc -> fn ({:tuple, _, t}) -> Nova.Set.union(acc, get_type_expr_deps(t)) end end, Nova.Set.empty, fields)
      {:ty_expr_for_all, _, t} -> get_type_expr_deps(t)
      {:ty_expr_constrained, cs, t} -> 
          constraint_deps = Nova.Runtime.foldl(fn acc -> fn c -> Nova.Set.union(acc, Nova.Set.singleton(c.class_name)) end end, Nova.Set.empty, cs)
          ty_deps = Nova.Runtime.foldl(fn acc -> fn c -> Nova.Set.union(acc, Nova.Runtime.foldl(fn acc2 -> fn ct -> Nova.Set.union(acc2, get_type_expr_deps(ct)) end end, Nova.Set.empty, c.types)) end end, Nova.Set.empty, cs)
          Nova.Set.union(constraint_deps, Nova.Set.union(ty_deps, get_type_expr_deps(t)))
      {:ty_expr_parens, t} -> get_type_expr_deps(t)
      {:ty_expr_tuple, ts} -> Nova.Runtime.foldl(fn acc -> fn t -> Nova.Set.union(acc, get_type_expr_deps(t)) end end, Nova.Set.empty, ts)
    end
  end

  def get_expr_deps(expr) do
    case expr do
      {:expr_var, name} -> Nova.Set.singleton(name)
      {:expr_qualified, ns, name} -> Nova.Set.singleton(Nova.Runtime.append(ns, Nova.Runtime.append(".", name)))
      {:expr_lit, _} -> Nova.Set.empty
      {:expr_app, e1, e2} -> Nova.Set.union(get_expr_deps(e1), get_expr_deps(e2))
      {:expr_lambda, pats, body} -> 
          bound_names = Nova.Runtime.foldl(fn acc -> fn p -> Nova.Set.union(acc, get_bound_names(p)) end end, Nova.Set.empty, pats)
          Nova.Set.difference(get_expr_deps(body), bound_names)
      {:expr_let, binds, body} -> 
          bind_deps = get_let_binds_deps(binds)
          bound_names = Nova.Runtime.foldl(fn acc -> fn b -> Nova.Set.union(acc, get_bound_names(b.pattern)) end end, Nova.Set.empty, binds)
          Nova.Set.difference(Nova.Set.union(bind_deps, get_expr_deps(body)), bound_names)
      {:expr_if, c, t, e} -> Nova.Set.union(get_expr_deps(c), Nova.Set.union(get_expr_deps(t), get_expr_deps(e)))
      {:expr_case, e, clauses} -> Nova.Set.union(get_expr_deps(e), Nova.Runtime.foldl(fn acc -> fn c -> Nova.Set.union(acc, get_case_clause_deps(c)) end end, Nova.Set.empty, clauses))
      {:expr_do, stmts} -> get_do_statements_deps(stmts)
      {:expr_bin_op, op, e1, e2} -> Nova.Set.union(Nova.Set.singleton(op), Nova.Set.union(get_expr_deps(e1), get_expr_deps(e2)))
      {:expr_unary_op, op, e} -> Nova.Set.union(Nova.Set.singleton(op), get_expr_deps(e))
      {:expr_list, es} -> Nova.Runtime.foldl(fn acc -> fn e -> Nova.Set.union(acc, get_expr_deps(e)) end end, Nova.Set.empty, es)
      {:expr_tuple, es} -> Nova.Runtime.foldl(fn acc -> fn e -> Nova.Set.union(acc, get_expr_deps(e)) end end, Nova.Set.empty, es)
      {:expr_record, fields} -> Nova.Runtime.foldl(fn acc -> fn ({:tuple, _, e}) -> Nova.Set.union(acc, get_expr_deps(e)) end end, Nova.Set.empty, fields)
      {:expr_record_access, e, _} -> get_expr_deps(e)
      {:expr_record_update, e, fields} -> Nova.Set.union(get_expr_deps(e), Nova.Runtime.foldl(fn acc -> fn ({:tuple, _, v}) -> Nova.Set.union(acc, get_expr_deps(v)) end end, Nova.Set.empty, fields))
      {:expr_typed, e, _} -> get_expr_deps(e)
      {:expr_parens, e} -> get_expr_deps(e)
      {:expr_section, _} -> Nova.Set.empty
    end
  end

  def get_let_binds_deps(binds) do
    Nova.Runtime.foldl(fn acc -> fn b -> Nova.Set.union(acc, get_expr_deps(b.value)) end end, Nova.Set.empty, binds)
  end

  def get_case_clause_deps(clause) do
    
      bound_names = get_bound_names(clause.pattern)
      body_deps = get_expr_deps(clause.body)
      guard_deps = case clause.guard do
        :nothing -> Nova.Set.empty
        {:just, g} -> get_expr_deps(g)
      end
      Nova.Set.difference(Nova.Set.union(body_deps, guard_deps), bound_names)
  end

  def get_do_statements_deps(stmts) do
    
      go = Nova.Runtime.fix2(fn go -> fn remaining -> fn bound -> case Nova.Array.uncons(remaining) do
        :nothing -> Nova.Set.empty
        {:just, %{head: stmt, tail: rest}} -> case stmt do
            {:do_let, binds} -> 
                deps = get_let_binds_deps(binds)
                new_bound = Nova.Runtime.foldl(fn acc -> fn b -> Nova.Set.union(acc, get_bound_names(b.pattern)) end end, bound, binds)
                Nova.Set.union(Nova.Set.difference(deps, bound), go.(rest).(new_bound))
            {:do_bind, pat, e} -> 
                deps = get_expr_deps(e)
                new_bound = Nova.Set.union(bound, get_bound_names(pat))
                Nova.Set.union(Nova.Set.difference(deps, bound), go.(rest).(new_bound))
            {:do_expr, e} -> Nova.Set.union(Nova.Set.difference(get_expr_deps(e), bound), go.(rest).(bound))
          end
      end  end end end)
      go.(stmts).(Nova.Set.empty)
  end

  def get_guarded_expr_deps(g) do
    
      guard_deps = Nova.Runtime.foldl(fn acc -> fn gc -> Nova.Set.union(acc, get_guard_clause_deps(gc)) end end, Nova.Set.empty, g.guards)
      body_deps = get_expr_deps(g.body)
      Nova.Set.union(guard_deps, body_deps)
  end

  def get_guard_clause_deps(gc) do
    case gc do
      {:guard_expr, e} -> get_expr_deps(e)
      {:guard_pat, pat, e} -> 
          bound_names = get_bound_names(pat)
          Nova.Set.difference(get_expr_deps(e), bound_names)
    end
  end

  def get_bound_names(pat) do
    case pat do
      {:pat_var, name} -> Nova.Set.singleton(name)
      :pat_wildcard -> Nova.Set.empty
      {:pat_lit, _} -> Nova.Set.empty
      {:pat_con, _, pats} -> Nova.Runtime.foldl(fn acc -> fn p -> Nova.Set.union(acc, get_bound_names(p)) end end, Nova.Set.empty, pats)
      {:pat_record, fields} -> Nova.Runtime.foldl(fn acc -> fn ({:tuple, _, p}) -> Nova.Set.union(acc, get_bound_names(p)) end end, Nova.Set.empty, fields)
      {:pat_list, pats} -> Nova.Runtime.foldl(fn acc -> fn p -> Nova.Set.union(acc, get_bound_names(p)) end end, Nova.Set.empty, pats)
      {:pat_cons, p1, p2} -> Nova.Set.union(get_bound_names(p1), get_bound_names(p2))
      {:pat_as, name, p} -> Nova.Set.insert(name, get_bound_names(p))
      {:pat_parens, p} -> get_bound_names(p)
    end
  end

  # @type dependency_graph :: %{forward: map()(decl_id())(set()(decl_id())), reverse: map()(decl_id())(set()(decl_id()))}

  def empty_graph() do
    %{forward: Nova.Map.empty, reverse: Nova.Map.empty}
  end

  def build_dependency_graph(decls, resolve_name) do
    
      ids = Nova.Map.keys(decls)
      forward = Nova.Runtime.foldl(fn acc -> fn id -> case Nova.Map.lookup(id, decls) do
        :nothing -> acc
        {:just, md} -> 
            deps = get_dependencies(md.decl)
            resolved_deps = Nova.Set.map_maybe(fn name -> resolve_name.(md.meta.namespace).(name) end, deps)
            Nova.Map.insert(id, resolved_deps, acc)
      end end end, Nova.Map.empty, ids)
      reverse = build_reverse_edges(forward)
      %{forward: forward, reverse: reverse}
  end

  def build_reverse_edges(forward) do
    
      entries = Nova.Map.to_unfoldable(forward)
      Nova.Runtime.foldl(fn acc -> fn ({:tuple, from_id, deps}) -> 
  deps_array = Nova.Set.to_unfoldable(deps)
  Nova.Runtime.foldl(fn acc2 -> fn to_id -> 
  existing = case Nova.Map.lookup(to_id, acc2) do
    :nothing -> Nova.Set.empty
    {:just, s} -> s
  end
  Nova.Map.insert(to_id, Nova.Set.insert(from_id, existing), acc2) end end, acc, deps_array) end end, Nova.Map.empty, entries)
  end

  def add_to_graph(decl_id, deps, graph) do
    
      forward = Nova.Map.insert(decl_id, deps, graph.forward)
      deps_array = Nova.Set.to_unfoldable(deps)
      reverse = Nova.Runtime.foldl(fn acc -> fn dep_id -> 
        existing = case Nova.Map.lookup(dep_id, acc) do
          :nothing -> Nova.Set.empty
          {:just, s} -> s
        end
        Nova.Map.insert(dep_id, Nova.Set.insert(decl_id, existing), acc) end end, graph.reverse, deps_array)
      %{forward: forward, reverse: reverse}
  end

  def remove_from_graph(decl_id, graph) do
    
      deps = case Nova.Map.lookup(decl_id, graph.forward) do
        :nothing -> Nova.Set.empty
        {:just, s} -> s
      end
      forward = Nova.Map.delete(decl_id, graph.forward)
      deps_array = Nova.Set.to_unfoldable(deps)
      reverse = Nova.Runtime.foldl(fn acc -> fn dep_id -> case Nova.Map.lookup(dep_id, acc) do
        :nothing -> acc
        {:just, dependents} -> Nova.Map.insert(dep_id, Nova.Set.delete(decl_id, dependents), acc)
      end end end, graph.reverse, deps_array)
      reverse_prime = Nova.Map.delete(decl_id, reverse)
      %{forward: forward, reverse: reverse_prime}
  end

  def get_dependents(graph, decl_id) do
    case Nova.Map.lookup(decl_id, graph.reverse) do
      :nothing -> Nova.Set.empty
      {:just, s} -> s
    end
  end

  def get_dependencies_of(graph, decl_id) do
    case Nova.Map.lookup(decl_id, graph.forward) do
      :nothing -> Nova.Set.empty
      {:just, s} -> s
    end
  end

  def get_affected(graph, decl_id) do
    
      go = Nova.Runtime.fix2(fn go -> fn to_process -> fn visited -> case Nova.Set.find_min(to_process) do
        :nothing -> visited
        {:just, id} -> if Nova.Set.member(id, visited) do
            go.(Nova.Set.delete(id, to_process)).(visited)
          else
            
              dependents = get_dependents(graph, id)
              new_to_process = Nova.Set.union(Nova.Set.delete(id, to_process), Nova.Set.difference(dependents, visited))
              go.(new_to_process).(Nova.Set.insert(id, visited))
          end
      end  end end end)
      go.(Nova.Set.singleton(decl_id)).(Nova.Set.empty)
  end

  def topological_sort(graph, ids) do
    
      initial_in_degree = Nova.Runtime.foldl(fn acc -> fn id -> 
        deps = get_dependencies_of(graph, id)
        relevant_deps = Nova.Set.size(Nova.Set.intersection(deps, Nova.Set.from_foldable(ids)))
        Nova.Map.insert(id, relevant_deps, acc) end end, Nova.Map.empty, ids)
      go = Nova.Runtime.fix3(fn go -> fn in_degree -> fn result -> fn queue -> case Nova.Array.uncons(queue) do
        :nothing -> result
        {:just, %{head: id, tail: rest_queue}} -> 
            all_dependents = Nova.Set.to_unfoldable(get_dependents(graph, id))
            dependents = Nova.Array.filter(fn d -> Nova.Array.elem(d, ids) end, all_dependents)
            new_in_degree = Nova.Runtime.foldl(fn acc -> fn dep -> case Nova.Map.lookup(dep, acc) do
              :nothing -> acc
              {:just, n} -> Nova.Map.insert(dep, (n - 1), acc)
            end end end, in_degree, dependents)
            new_queue = Nova.Runtime.append(rest_queue, Nova.Array.filter(fn d -> case Nova.Map.lookup(d, new_in_degree) do
  :nothing -> false
  {:just, n} -> ((n == 0) and (not(Nova.Array.elem(d, result)) and not(Nova.Array.elem(d, queue))))
end end, dependents))
            go.(new_in_degree).(Nova.Array.snoc(result, id)).(new_queue)
      end  end end end end)
      initial_queue = Nova.Array.filter(fn id -> case Nova.Map.lookup(id, initial_in_degree) do
        :nothing -> false
        {:just, n} -> (n == 0)
      end end, ids)
      go.(initial_in_degree).([]).(initial_queue)
  end
end
