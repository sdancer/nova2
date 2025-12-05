defmodule Nova.Dependencies do
  @moduledoc """
  Dependency extraction and graph management for Nova declarations.

  This module provides functions to:
  - Extract free names referenced by declarations
  - Build and maintain dependency graphs
  - Find affected declarations when one changes
  - Topologically sort declarations for type checking order
  """

  # ============================================================================
  # Dependency Extraction
  # ============================================================================

  # Convert PureScript-style linked list to Elixir list
  # Note: Support both :nil and :nil_ (with underscore) since code generator escapes 'nil' as reserved word
  defp to_list(:nil), do: []
  defp to_list(:nil_), do: []
  defp to_list({:cons, h, t}), do: [h | to_list(t)]
  defp to_list(list) when is_list(list), do: list

  @doc """
  Extract all free names referenced by a declaration.
  These are names that need to be resolved from the environment.
  """
  def get_dependencies(decl) do
    case decl do
      {:decl_function, f} ->
        body_deps = get_expr_deps(f.body)
        guard_deps = Enum.reduce(to_list(f.guards), MapSet.new(), fn g, acc ->
          MapSet.union(acc, get_guarded_expr_deps(g))
        end)
        param_names = Enum.reduce(to_list(f.parameters), MapSet.new(), fn p, acc ->
          MapSet.union(acc, get_bound_names(p))
        end)
        MapSet.difference(MapSet.union(body_deps, guard_deps), param_names)

      {:decl_data_type, d} ->
        Enum.reduce(to_list(d.constructors), MapSet.new(), fn c, acc ->
          MapSet.union(acc, get_constructor_deps(c))
        end)

      {:decl_type_alias, a} ->
        get_type_expr_deps(a.ty)

      {:decl_type_class, c} ->
        Enum.reduce(to_list(c.methods), MapSet.new(), fn m, acc ->
          MapSet.union(acc, get_type_expr_deps(m.ty))
        end)

      {:decl_type_class_instance, i} ->
        ty_deps = get_type_expr_deps(i.ty)
        method_deps = Enum.reduce(to_list(i.methods), MapSet.new(), fn m, acc ->
          MapSet.union(acc, get_expr_deps(m.body))
        end)
        MapSet.union(ty_deps, method_deps)

      {:decl_foreign_import, f} ->
        get_type_expr_deps(f.type_signature)

      {:decl_type_sig, s} ->
        get_type_expr_deps(s.ty)

      {:decl_type, t} ->
        get_type_expr_deps(t.type_signature)

      {:decl_module, m} ->
        Enum.reduce(to_list(m.declarations), MapSet.new(), fn d, acc ->
          MapSet.union(acc, get_dependencies(d))
        end)

      {:decl_import, _} ->
        MapSet.new()

      _ ->
        MapSet.new()
    end
  end

  # Get dependencies from a data constructor
  defp get_constructor_deps(c) do
    Enum.reduce(to_list(c.fields), MapSet.new(), fn field, acc ->
      MapSet.union(acc, get_type_expr_deps(field.ty))
    end)
  end

  # Get dependencies from a type expression
  defp get_type_expr_deps(ty) do
    case ty do
      {:ty_expr_con, name} -> MapSet.new([name])
      {:ty_expr_var, _} -> MapSet.new()
      {:ty_expr_app, t1, t2} -> MapSet.union(get_type_expr_deps(t1), get_type_expr_deps(t2))
      {:ty_expr_arrow, t1, t2} -> MapSet.union(get_type_expr_deps(t1), get_type_expr_deps(t2))
      {:ty_expr_record, fields, _row} ->
        Enum.reduce(to_list(fields), MapSet.new(), fn {:tuple, _, t}, acc ->
          MapSet.union(acc, get_type_expr_deps(t))
        end)
      {:ty_expr_for_all, _, t} -> get_type_expr_deps(t)
      {:ty_expr_constrained, cs, t} ->
        constraint_deps = Enum.reduce(to_list(cs), MapSet.new(), fn c, acc ->
          class_dep = MapSet.new([c.class_name])
          type_deps = Enum.reduce(to_list(c.types), MapSet.new(), fn ct, acc2 ->
            MapSet.union(acc2, get_type_expr_deps(ct))
          end)
          MapSet.union(acc, MapSet.union(class_dep, type_deps))
        end)
        MapSet.union(constraint_deps, get_type_expr_deps(t))
      {:ty_expr_parens, t} -> get_type_expr_deps(t)
      {:ty_expr_tuple, ts} ->
        Enum.reduce(to_list(ts), MapSet.new(), fn t, acc ->
          MapSet.union(acc, get_type_expr_deps(t))
        end)
      _ -> MapSet.new()
    end
  end

  # Get free variable dependencies from an expression
  defp get_expr_deps(expr) do
    case expr do
      {:expr_var, name} -> MapSet.new([name])
      {:expr_qualified, ns, name} -> MapSet.new(["#{ns}.#{name}"])
      {:expr_lit, _} -> MapSet.new()
      {:expr_app, e1, e2} -> MapSet.union(get_expr_deps(e1), get_expr_deps(e2))
      {:expr_lambda, pats, body} ->
        bound_names = Enum.reduce(to_list(pats), MapSet.new(), fn p, acc ->
          MapSet.union(acc, get_bound_names(p))
        end)
        MapSet.difference(get_expr_deps(body), bound_names)
      {:expr_let, binds, body} ->
        binds_list = to_list(binds)
        bind_deps = get_let_binds_deps(binds_list)
        bound_names = Enum.reduce(binds_list, MapSet.new(), fn b, acc ->
          MapSet.union(acc, get_bound_names(b.pattern))
        end)
        MapSet.difference(MapSet.union(bind_deps, get_expr_deps(body)), bound_names)
      {:expr_if, c, t, e} ->
        MapSet.union(get_expr_deps(c), MapSet.union(get_expr_deps(t), get_expr_deps(e)))
      {:expr_case, scrutinee, clauses} ->
        clause_deps = Enum.reduce(to_list(clauses), MapSet.new(), fn c, acc ->
          MapSet.union(acc, get_case_clause_deps(c))
        end)
        MapSet.union(get_expr_deps(scrutinee), clause_deps)
      {:expr_do, stmts} -> get_do_statements_deps(stmts)
      {:expr_bin_op, op, e1, e2} ->
        MapSet.union(MapSet.new([op]), MapSet.union(get_expr_deps(e1), get_expr_deps(e2)))
      {:expr_unary_op, op, e} ->
        MapSet.union(MapSet.new([op]), get_expr_deps(e))
      {:expr_list, es} ->
        Enum.reduce(to_list(es), MapSet.new(), fn e, acc -> MapSet.union(acc, get_expr_deps(e)) end)
      {:expr_tuple, es} ->
        Enum.reduce(to_list(es), MapSet.new(), fn e, acc -> MapSet.union(acc, get_expr_deps(e)) end)
      {:expr_record, fields} ->
        Enum.reduce(to_list(fields), MapSet.new(), fn {:tuple, _, e}, acc ->
          MapSet.union(acc, get_expr_deps(e))
        end)
      {:expr_record_access, e, _} -> get_expr_deps(e)
      {:expr_record_update, e, fields} ->
        field_deps = Enum.reduce(to_list(fields), MapSet.new(), fn {:tuple, _, v}, acc ->
          MapSet.union(acc, get_expr_deps(v))
        end)
        MapSet.union(get_expr_deps(e), field_deps)
      {:expr_typed, e, _} -> get_expr_deps(e)
      {:expr_parens, e} -> get_expr_deps(e)
      {:expr_section, _} -> MapSet.new()
      _ -> MapSet.new()
    end
  end

  defp get_let_binds_deps(binds) do
    Enum.reduce(binds, MapSet.new(), fn b, acc ->
      MapSet.union(acc, get_expr_deps(b.value))
    end)
  end

  defp get_case_clause_deps(clause) do
    bound_names = get_bound_names(clause.pattern)
    body_deps = get_expr_deps(clause.body)
    guard_deps = case clause.guard do
      nil -> MapSet.new()
      :nothing -> MapSet.new()
      {:just, g} -> get_expr_deps(g)
      g -> get_expr_deps(g)
    end
    MapSet.difference(MapSet.union(body_deps, guard_deps), bound_names)
  end

  defp get_do_statements_deps(stmts) do
    {deps, _bound} = Enum.reduce(to_list(stmts), {MapSet.new(), MapSet.new()}, fn stmt, {acc_deps, bound} ->
      case stmt do
        {:do_let, binds} ->
          binds_list = to_list(binds)
          deps = get_let_binds_deps(binds_list)
          new_bound = Enum.reduce(binds_list, bound, fn b, acc ->
            MapSet.union(acc, get_bound_names(b.pattern))
          end)
          {MapSet.union(acc_deps, MapSet.difference(deps, bound)), new_bound}
        {:do_bind, pat, e} ->
          deps = get_expr_deps(e)
          new_bound = MapSet.union(bound, get_bound_names(pat))
          {MapSet.union(acc_deps, MapSet.difference(deps, bound)), new_bound}
        {:do_expr, e} ->
          deps = get_expr_deps(e)
          {MapSet.union(acc_deps, MapSet.difference(deps, bound)), bound}
        _ ->
          {acc_deps, bound}
      end
    end)
    deps
  end

  defp get_guarded_expr_deps(g) do
    guard_deps = Enum.reduce(to_list(g.guards), MapSet.new(), fn gc, acc ->
      MapSet.union(acc, get_guard_clause_deps(gc))
    end)
    MapSet.union(guard_deps, get_expr_deps(g.body))
  end

  defp get_guard_clause_deps(gc) do
    case gc do
      {:guard_expr, e} -> get_expr_deps(e)
      {:guard_pat, pat, e} ->
        bound_names = get_bound_names(pat)
        MapSet.difference(get_expr_deps(e), bound_names)
      _ -> MapSet.new()
    end
  end

  # Get names bound by a pattern (to exclude from dependencies)
  defp get_bound_names(pat) do
    case pat do
      {:pat_var, name} -> MapSet.new([name])
      :pat_wildcard -> MapSet.new()
      {:pat_lit, _} -> MapSet.new()
      {:pat_con, _, pats} ->
        Enum.reduce(to_list(pats), MapSet.new(), fn p, acc -> MapSet.union(acc, get_bound_names(p)) end)
      {:pat_record, fields} ->
        Enum.reduce(to_list(fields), MapSet.new(), fn {:tuple, _, p}, acc ->
          MapSet.union(acc, get_bound_names(p))
        end)
      {:pat_list, pats} ->
        Enum.reduce(to_list(pats), MapSet.new(), fn p, acc -> MapSet.union(acc, get_bound_names(p)) end)
      {:pat_cons, p1, p2} -> MapSet.union(get_bound_names(p1), get_bound_names(p2))
      {:pat_as, name, p} -> MapSet.put(get_bound_names(p), name)
      {:pat_parens, p} -> get_bound_names(p)
      _ -> MapSet.new()
    end
  end

  # ============================================================================
  # Dependency Graph Operations
  # ============================================================================

  @doc """
  Create an empty dependency graph.
  """
  def empty_graph do
    %{forward: %{}, reverse: %{}}
  end

  @doc """
  Add a declaration to the graph with its dependencies.
  """
  def add_to_graph(graph, decl_id, deps) do
    # Add forward edge
    forward = Map.put(graph.forward, decl_id, deps)

    # Add reverse edges
    reverse = Enum.reduce(deps, graph.reverse, fn dep_id, acc ->
      existing = Map.get(acc, dep_id, MapSet.new())
      Map.put(acc, dep_id, MapSet.put(existing, decl_id))
    end)

    %{forward: forward, reverse: reverse}
  end

  @doc """
  Remove a declaration from the graph.
  """
  def remove_from_graph(graph, decl_id) do
    # Get what this declaration depended on
    deps = Map.get(graph.forward, decl_id, MapSet.new())

    # Remove from forward
    forward = Map.delete(graph.forward, decl_id)

    # Remove from reverse edges of dependencies
    reverse = Enum.reduce(deps, graph.reverse, fn dep_id, acc ->
      case Map.get(acc, dep_id) do
        nil -> acc
        dependents -> Map.put(acc, dep_id, MapSet.delete(dependents, decl_id))
      end
    end)

    # Also remove decl_id's own reverse entry
    reverse = Map.delete(reverse, decl_id)

    %{forward: forward, reverse: reverse}
  end

  @doc """
  Get all declarations that depend on a given declaration (direct dependents).
  """
  def get_dependents(graph, decl_id) do
    Map.get(graph.reverse, decl_id, MapSet.new())
  end

  @doc """
  Get all declarations that a given declaration depends on.
  """
  def get_dependencies_of(graph, decl_id) do
    Map.get(graph.forward, decl_id, MapSet.new())
  end

  @doc """
  Get all transitively affected declarations when a declaration changes.
  This includes direct dependents and their dependents, recursively.
  """
  def get_affected(graph, decl_id) do
    do_get_affected(graph, MapSet.new([decl_id]), MapSet.new())
  end

  defp do_get_affected(_graph, to_process, visited) when map_size(to_process) == 0 do
    visited
  end

  defp do_get_affected(graph, to_process, visited) do
    # Pop one element from to_process
    [id | rest] = MapSet.to_list(to_process)
    to_process = MapSet.new(rest)

    if MapSet.member?(visited, id) do
      do_get_affected(graph, to_process, visited)
    else
      dependents = get_dependents(graph, id)
      new_to_process = MapSet.union(to_process, MapSet.difference(dependents, visited))
      do_get_affected(graph, new_to_process, MapSet.put(visited, id))
    end
  end

  @doc """
  Topological sort of declarations (for type checking order).
  Returns declarations in dependency order (dependencies before dependents).
  """
  def topological_sort(graph, ids) do
    id_set = MapSet.new(ids)

    # Calculate in-degree for each id (only counting dependencies within our set)
    initial_in_degree = Enum.reduce(ids, %{}, fn id, acc ->
      deps = get_dependencies_of(graph, id)
      relevant_deps = MapSet.intersection(deps, id_set)
      Map.put(acc, id, MapSet.size(relevant_deps))
    end)

    # Find initial nodes with no dependencies (in-degree 0)
    initial_queue = Enum.filter(ids, fn id ->
      Map.get(initial_in_degree, id, 0) == 0
    end)

    do_topo_sort(graph, id_set, initial_in_degree, [], initial_queue)
  end

  defp do_topo_sort(_graph, _id_set, _in_degree, result, []) do
    Enum.reverse(result)
  end

  defp do_topo_sort(graph, id_set, in_degree, result, [id | rest_queue]) do
    # Get dependents of this id that are in our set
    all_dependents = get_dependents(graph, id)
    dependents = MapSet.intersection(all_dependents, id_set) |> MapSet.to_list()

    # Decrease in-degree for each dependent
    new_in_degree = Enum.reduce(dependents, in_degree, fn dep, acc ->
      Map.update(acc, dep, 0, fn n -> n - 1 end)
    end)

    # Add dependents with in-degree 0 to queue
    new_queue = rest_queue ++ Enum.filter(dependents, fn d ->
      Map.get(new_in_degree, d, 0) == 0 and
      d not in result and
      d not in rest_queue
    end)

    do_topo_sort(graph, id_set, new_in_degree, [id | result], new_queue)
  end

  @doc """
  Resolve a name to a declaration ID given a namespace context.
  Returns the decl_id if found, nil otherwise.
  """
  def resolve_name(state, namespace, name) do
    # First try unqualified name in current namespace
    case Map.get(state.namespaces, namespace) do
      nil -> nil
      ns ->
        case Map.get(ns.name_index, name) do
          nil ->
            # Try qualified name (might be from import)
            if String.contains?(name, ".") do
              [imported_ns | rest] = String.split(name, ".", parts: 2)
              local_name = Enum.join(rest, ".")
              case Map.get(state.namespaces, imported_ns) do
                nil -> nil
                imported -> Map.get(imported.name_index, local_name)
              end
            else
              # Try to find in imports
              find_in_imports(state, ns.imports, name)
            end
          decl_id -> decl_id
        end
    end
  end

  defp find_in_imports(_state, [], _name), do: nil
  defp find_in_imports(state, [import_ns | rest], name) do
    case Map.get(state.namespaces, import_ns) do
      nil -> find_in_imports(state, rest, name)
      ns ->
        case Map.get(ns.name_index, name) do
          nil -> find_in_imports(state, rest, name)
          decl_id -> decl_id
        end
    end
  end
end
