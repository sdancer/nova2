defmodule Nova.Compiler.CstToAst do
  # import Prelude

  # import Data.List

  # import Data.List

  # import Data.Maybe

  # import Data.Either

  # import Data.Tuple

  # import Nova.Compiler.Cst

  # import Nova.Compiler.Ast

  # @type separated_data_ctor :: %{head: data_ctor()(void()), tail: list()((tuple()(source_token())((data_ctor()(void())))))}

  # @type separated_instance :: %{head: instance()(void()), tail: list()((tuple()(source_token())((instance()(void())))))}

  # @type separated_pattern_guard :: %{head: pattern_guard()(void()), tail: list()((tuple()(source_token())((pattern_guard()(void())))))}

  # @type separated_binder :: %{head: binder()(void()), tail: list()((tuple()(source_token())((binder()(void())))))}

  # @type labeled_ident_type :: %{label: name()(ident()), separator: source_token(), value: type()(void())}

  # @type wrapped_expr :: %{open: source_token(), value: expr()(void()), close: source_token()}

  # @type cst_import_decl :: import_decl()(void())

  # @type cst_separated_import :: %{head: import_()(void()), tail: list()((tuple()(source_token())((import_()(void())))))}



  def unwrap_ident(({:ident, s})) do
    s
  end



  def unwrap_proper(({:proper, s})) do
    s
  end



  def unwrap_label(({:label, s})) do
    s
  end



  def unwrap_operator(({:operator, s})) do
    s
  end



  def unwrap_module_name(({:module_name, s})) do
    s
  end



  def extract_type_var(({:type_var_kinded, wrapped})) do
    unwrap_ident(wrapped.value.label.name)
  end

  def extract_type_var(({:type_var_name, name})) do
    unwrap_ident(name.name)
  end



  def int_value_to_int(({:small_int, n})) do
    n
  end

  def int_value_to_int(({:big_int, _})) do
    0
  end



  def snd(({:tuple, _, b})) do
    b
  end



  def traverse(f, lst) do
    case lst do
      :nil -> {:right, []}
      ([head | tail]) ->     Nova.Runtime.bind(f.(head), fn h ->
      case traverse(f, tail) do
        {:left, err} -> {:left, err}
        {:right, t} ->
          [((&Nova.Runtime.pure/1)).(h) | t]
      end
    end)
    end
  end



  def list_map_with_index(f, list) do
    
      go = Nova.Runtime.fix2(fn go -> fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {_, :nil} -> []
        {i, ([x | xs])} -> [f.(i).(x) | go.(((i + 1))).(xs)]
      end end end end)
      go.(0).(list)
  end



  def is_underscore(({:expr_ident, qn})) do
    (unwrap_ident(qn.name) == "_")
  end

  def is_underscore(_) do
    false
  end



  def convert_module(cst_mod) do
        module_name = unwrap_module_name(cst_mod.header.name.name)
  case traverse((&convert_import_decl/1), cst_mod.header.imports) do
    {:left, err} -> {:left, err}
    {:right, imports} ->
      case convert_declarations(cst_mod.body.decls) do
        {:left, err} -> {:left, err}
        {:right, decls} ->
                    import_decls = Nova.Runtime.map(fn a -> Nova.Compiler.Ast.decl_import(a) end, imports)
          Nova.Runtime.pure(%{name: module_name, declarations: Nova.Runtime.append(import_decls, decls)})
      end
  end
  end



  def convert_import_decl(imp) do
        module_name = unwrap_module_name(imp.module.name)
    alias_ = Nova.Runtime.map((fn ({:tuple, _, qual_name}) -> unwrap_module_name(qual_name.name) end), imp.qualified)
  Nova.Runtime.bind(case imp.names do
  :nothing -> Nova.Runtime.pure([])
  {:just, ({:tuple, _, delim})} ->   case convert_import_items(delim.value) do
    {:left, err} -> {:left, err}
    {:right, import_items} ->
      Nova.Runtime.pure(import_items)
  end
end, fn items ->
        is_hiding = case imp.names do
          {:just, ({:tuple, ({:just, _}), _})} -> true
          _ -> false
        end
    Nova.Runtime.pure(%{module_name: module_name, alias_: alias_, items: items, hiding: is_hiding})
  end)
  end



  def convert_import_items(sep) do
        all_imports = [sep.head | (Nova.Runtime.map((fn ({:tuple, _, i}) -> i end), sep.tail))]
  traverse((&convert_import_item/1), all_imports)
  end



  def convert_import_item(imp) do
    case imp do
      {:import_value, name} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.import_value((unwrap_ident(name.name))))
      {:import_op, name} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.import_value((unwrap_operator(name.name))))
      {:import_type, name, m_members} -> 
          type_name = unwrap_proper(name.name)
          spec = convert_data_members(m_members)
          ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.import_type(type_name, spec))
      {:import_type_op, _, name} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.import_value((unwrap_operator(name.name))))
      {:import_class, _, name} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.import_type((unwrap_proper(name.name)), Nova.Compiler.Ast.import_none))
      {:import_error, _} -> {:left, "Cannot convert import error"}
    end
  end



  def convert_data_members(:nothing) do
    Nova.Compiler.Ast.import_none
  end

  def convert_data_members(({:just, ({:data_all, _})})) do
    Nova.Compiler.Ast.import_all
  end

  def convert_data_members(({:just, ({:data_enumerated, delim})})) do
    case delim.value do
      :nothing -> Nova.Compiler.Ast.import_none
      {:just, sep} -> 
          names = [(unwrap_proper(sep.head.name)) | (Nova.Runtime.map((fn ({:tuple, _, n}) -> unwrap_proper(n.name) end), sep.tail))]
          Nova.Compiler.Ast.import_some(names)
    end
  end



  def convert_declarations(decls) do
    traverse((&convert_declaration/1), decls)
  end



  def convert_declaration(decl) do
    case decl do
      {:decl_data, head, ctors} -> convert_data_type(head, ctors)
      {:decl_type, head, _, ty} -> convert_type_alias(head, ty)
      {:decl_newtype, head, _, ctor_name, wrapped_ty} -> convert_newtype(head, ctor_name, wrapped_ty)
      {:decl_class, head, methods} -> convert_type_class(head, methods)
      {:decl_instance_chain, chain} -> convert_instance(chain)
      {:decl_derive, _, _, inst_head} -> convert_derive_instance(inst_head)
      {:decl_signature, labeled} -> convert_type_signature(labeled)
      {:decl_value, vbf} -> convert_function(vbf)
      {:decl_fixity, fields} -> convert_fixity(fields)
      {:decl_foreign, _, _, foreign_prime} -> convert_foreign(foreign_prime)
      {:decl_error, _} -> {:left, "Cannot convert error declaration"}
    end
  end



  def convert_data_type(head, m_ctors) do
        name = unwrap_proper(head.name.name)
    type_vars = Nova.Runtime.map((&extract_type_var/1), head.vars)
  Nova.Runtime.bind(case m_ctors do
  :nothing -> Nova.Runtime.pure([])
  {:just, ({:tuple, _, separated})} -> convert_data_ctors(separated)
end, fn ctors ->
    ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_data_type(%{name: name, type_vars: type_vars, constructors: ctors}))
  end)
  end



  def convert_data_ctors(sep) do
        all_ctors = [sep.head | (Nova.Runtime.map((fn ({:tuple, _, c}) -> c end), sep.tail))]
  traverse((&convert_data_ctor/1), all_ctors)
  end



  def convert_data_ctor(ctor) do
        name = unwrap_proper(ctor.name.name)
  case traverse((&convert_type_to_field/1), ctor.fields) do
    {:left, err} -> {:left, err}
    {:right, fields} ->
      Nova.Runtime.pure(%{name: name, fields: fields, is_record: false})
  end
  end



  def convert_type_to_field(ty) do
      case convert_type(ty) do
    {:left, err} -> {:left, err}
    {:right, ty_expr} ->
      Nova.Runtime.pure(%{label: "", ty: ty_expr})
  end
  end



  def convert_type_alias(head, ty) do
        name = unwrap_proper(head.name.name)
    type_vars = Nova.Runtime.map((&extract_type_var/1), head.vars)
  case convert_type(ty) do
    {:left, err} -> {:left, err}
    {:right, ty_expr} ->
      ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_type_alias(%{name: name, type_vars: type_vars, ty: ty_expr}))
  end
  end



  def convert_newtype(head, ctor_name, wrapped_ty) do
        name = unwrap_proper(head.name.name)
    type_vars = Nova.Runtime.map((&extract_type_var/1), head.vars)
    constructor = unwrap_proper(ctor_name.name)
  case convert_type(wrapped_ty) do
    {:left, err} -> {:left, err}
    {:right, wrapped_type} ->
      ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_newtype(%{name: name, type_vars: type_vars, constructor: constructor, wrapped_type: wrapped_type}))
  end
  end



  def convert_type_class(head, m_methods) do
        name = unwrap_proper(head.name.name)
    type_vars = Nova.Runtime.map((&extract_type_var/1), head.vars)
  Nova.Runtime.bind(case m_methods do
  :nothing -> Nova.Runtime.pure([])
  {:just, ({:tuple, _, labeled})} -> traverse((&convert_method_sig/1), labeled)
end, fn methods ->
    ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_type_class(%{name: name, type_vars: type_vars, methods: methods, kind: :nothing}))
  end)
  end



  def convert_method_sig(labeled) do
        name = unwrap_ident(labeled.label.name)
  case convert_type(labeled.value) do
    {:left, err} -> {:left, err}
    {:right, ty} ->
      Nova.Runtime.pure(%{name: name, type_vars: [], constraints: [], ty: ty})
  end
  end



  def convert_instance(chain) do
        inst = chain.head
    class_name = unwrap_proper(inst.head.class_name.name)
  Nova.Runtime.bind(case inst.head.types do
  ([t | _]) -> convert_type(t)
  :nil -> {:left, "Instance needs at least one type"}
end, fn ty ->
    Nova.Runtime.bind(case inst.body do
  :nothing -> Nova.Runtime.pure([])
  {:just, ({:tuple, _, bindings})} -> traverse((&convert_instance_binding/1), bindings)
end, fn methods ->
      ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_type_class_instance(%{class_name: class_name, ty: ty, methods: methods, derived: false}))
    end)
  end)
  end



  def convert_instance_binding(binding) do
    case binding do
      {:instance_binding_signature, _} -> {:left, "Type signatures in instances not yet supported"}
      {:instance_binding_name, vbf} -> convert_value_binding(vbf)
    end
  end



  def convert_derive_instance(head) do
        class_name = unwrap_proper(head.class_name.name)
  Nova.Runtime.bind(case head.types do
  ([t | _]) -> convert_type(t)
  :nil -> {:left, "Derived instance needs at least one type"}
end, fn ty ->
    ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_type_class_instance(%{class_name: class_name, ty: ty, methods: [], derived: true}))
  end)
  end



  def convert_function(vbf) do
      case convert_value_binding(vbf) do
    {:left, err} -> {:left, err}
    {:right, func} ->
      ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_function(func))
  end
  end



  def convert_value_binding(vbf) do
        name = unwrap_ident(vbf.name.name)
  case traverse((&convert_binder/1), vbf.binders) do
    {:left, err} -> {:left, err}
    {:right, params} ->
      case vbf.guarded do
  {:unconditional, _, wh} ->   case convert_where_expr(wh) do
    {:left, err} -> {:left, err}
    {:right, body} ->
      Nova.Runtime.pure(%{name: name, parameters: params, body: body, guards: [], type_signature: :nothing})
  end
  {:guarded, guarded_exprs} ->   case traverse((&convert_guarded_expr/1), guarded_exprs) do
    {:left, err} -> {:left, err}
    {:right, guards} ->
      case guards do
  ([g | _]) -> Nova.Runtime.pure(%{name: name, parameters: params, body: g.body, guards: guards, type_signature: :nothing})
  :nil -> {:left, "Empty guarded expression"}
end
  end
end
  end
  end



  def convert_where_expr(wh) do
      case convert_expr(wh.expr) do
    {:left, err} -> {:left, err}
    {:right, expr} ->
      case wh.bindings do
  :nothing -> Nova.Runtime.pure(expr)
  {:just, ({:tuple, _, bindings})} ->   case convert_where_bindings(bindings) do
    {:left, err} -> {:left, err}
    {:right, let_binds} ->
      ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_let(let_binds, expr))
  end
end
  end
  end



  def convert_where_bindings(bindings) do
    
      get_value_binding = fn auto_arg0 -> case auto_arg0 do
        ({:let_binding_name, vbf}) -> {:just, vbf}
        _ -> :nothing
      end end
      convert_let_bind = fn vbf ->       bind_name = unwrap_ident(vbf.name.name)
   case traverse((&convert_binder/1), vbf.binders) do
     {:left, err} -> {:left, err}
     {:right, bind_params} ->
       case vbf.guarded do
  {:unconditional, _, wh} ->   case convert_where_expr(wh) do
    {:left, err} -> {:left, err}
    {:right, bind_body} ->
            value = case bind_params do
              :nil -> bind_body
              _ -> Nova.Compiler.Ast.expr_lambda(bind_params, bind_body)
            end
      Nova.Runtime.pure(%{pattern: Nova.Compiler.Ast.pat_var(bind_name), value: value, type_ann: :nothing})
  end
  {:guarded, _} -> {:left, "Guarded where bindings not yet supported"}
end
   end end
      value_bindings = Nova.List.map_maybe(get_value_binding, bindings)
traverse(convert_let_bind, value_bindings)
  end



  def convert_guarded_expr(ge) do
      case convert_pattern_guards(ge.patterns) do
    {:left, err} -> {:left, err}
    {:right, guards} ->
      case convert_where_expr(ge.where) do
        {:left, err} -> {:left, err}
        {:right, body} ->
          Nova.Runtime.pure(%{guards: guards, body: body})
      end
  end
  end



  def convert_pattern_guards(sep) do
        all_guards = [sep.head | (Nova.Runtime.map((fn ({:tuple, _, g}) -> g end), sep.tail))]
  traverse((&convert_pattern_guard/1), all_guards)
  end



  def convert_pattern_guard(pg) do
    case pg.binder do
      :nothing ->     case convert_expr(pg.expr) do
      {:left, err} -> {:left, err}
      {:right, expr} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.guard_expr(expr))
    end
      {:just, ({:tuple, binder, _})} ->     case convert_binder(binder) do
      {:left, err} -> {:left, err}
      {:right, pat} ->
        case convert_expr(pg.expr) do
          {:left, err} -> {:left, err}
          {:right, expr} ->
            ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.guard_pat(pat, expr))
        end
    end
    end
  end



  def convert_type_signature(labeled) do
        name = unwrap_ident(labeled.label.name)
  Nova.Runtime.bind(extract_constraints(labeled.value), fn {:tuple, constraints, ty} ->
    case convert_type(ty) do
      {:left, err} -> {:left, err}
      {:right, ty_expr} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_type_sig(%{name: name, type_vars: [], constraints: constraints, ty: ty_expr}))
    end
  end)
  end



  def extract_constraints(ty) do
    case ty do
      {:type_constrained, constraint, _, rest} ->     case convert_constraint_type(constraint) do
      {:left, err} -> {:left, err}
      {:right, c} ->
        Nova.Runtime.bind(extract_constraints(rest), fn {:tuple, rest_cs, rest_ty} ->
          ((&Nova.Runtime.pure/1)).({:tuple, ([c | rest_cs]), rest_ty})
        end)
    end
      _ -> ((&Nova.Runtime.pure/1)).({:tuple, [], ty})
    end
  end



  def convert_constraint_type(ty) do
    case ty do
      {:type_constructor, qn} ->         class_name = unwrap_proper(qn.name)
    Nova.Runtime.pure(%{class_name: class_name, types: []})
      {:type_app, ({:type_constructor, qn}), args} ->         class_name = unwrap_proper(qn.name)
    case traverse((&convert_type/1), args) do
      {:left, err} -> {:left, err}
      {:right, types} ->
        Nova.Runtime.pure(%{class_name: class_name, types: types})
    end
      _ -> {:left, "Invalid constraint type"}
    end
  end



  def convert_fixity(fields) do
        assoc = case (snd(fields.keyword)) do
      :infix -> Nova.Compiler.Ast.assoc_none
      :infixl -> Nova.Compiler.Ast.assoc_left
      :infixr -> Nova.Compiler.Ast.assoc_right
    end
    prec = snd(fields.prec)
  case fields.operator do
  {:fixity_value, qn, _, op_name} ->     function_name = case qn.name do
      {:left, ident} -> unwrap_ident(ident)
      {:right, proper} -> unwrap_proper(proper)
    end
    operator = unwrap_operator(op_name.name)
  ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_infix(%{associativity: assoc, precedence: prec, function_name: function_name, operator: operator}))
  {:fixity_type, _, qn, _, op_name} ->     function_name = unwrap_proper(qn.name)
    operator = unwrap_operator(op_name.name)
  ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_infix(%{associativity: assoc, precedence: prec, function_name: function_name, operator: operator}))
end
  end



  def convert_foreign(foreign_prime) do
    case foreign_prime do
      {:foreign_value, labeled} ->         function_name = unwrap_ident(labeled.label.name)
    case convert_type(labeled.value) do
      {:left, err} -> {:left, err}
      {:right, ty} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_foreign_import(%{module_name: "", function_name: function_name, alias_: :nothing, type_signature: ty}))
    end
      {:foreign_data, _, labeled} ->         name = unwrap_proper(labeled.label.name)
    case convert_type(labeled.value) do
      {:left, err} -> {:left, err}
      {:right, ty} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.decl_type_alias(%{name: name, type_vars: [], ty: ty}))
    end
    end
  end



  def convert_type(ty) do
    case ty do
      {:type_var, name} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_var((unwrap_ident(name.name))))
      {:type_constructor, qn} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_con((unwrap_proper(qn.name))))
      {:type_wildcard, _} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_var("_"))
      {:type_hole, name} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_var((Nova.Runtime.append("?", unwrap_ident(name.name)))))
      {:type_string, _, s} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_con((Nova.Runtime.append(Nova.Runtime.append("\"", s), "\""))))
      {:type_int, _, _} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_con("Int"))
      {:type_row, wrapped} ->     case convert_row(wrapped.value) do
      {:left, err} -> {:left, err}
      {:right, row} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_record(row.fields, row.tail))
    end
      {:type_record, wrapped} ->     case convert_row(wrapped.value) do
      {:left, err} -> {:left, err}
      {:right, row} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_record(row.fields, row.tail))
    end
      {:type_forall, _, vars, _, body} ->         var_names = Nova.Runtime.map((&extract_type_var/1), vars)
    case convert_type(body) do
      {:left, err} -> {:left, err}
      {:right, body_ty} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_for_all(var_names, body_ty))
    end
      {:type_kinded, t, _, _} -> convert_type(t)
      {:type_app, head, args} ->     case convert_type(head) do
      {:left, err} -> {:left, err}
      {:right, head_ty} ->
        case traverse((&convert_type/1), args) do
          {:left, err} -> {:left, err}
          {:right, arg_tys} ->
            ((&Nova.Runtime.pure/1)).(Nova.List.foldl(fn a, b -> Nova.Compiler.Ast.ty_expr_app(a, b) end, head_ty, arg_tys))
        end
    end
      {:type_op, head, ops} ->     case convert_type(head) do
      {:left, err} -> {:left, err}
      {:right, head_ty} ->
        fold_type_ops(head_ty, ops)
    end
      {:type_arrow, from, _, to} ->     case convert_type(from) do
      {:left, err} -> {:left, err}
      {:right, from_ty} ->
        case convert_type(to) do
          {:left, err} -> {:left, err}
          {:right, to_ty} ->
            ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_arrow(from_ty, to_ty))
        end
    end
      {:type_constrained, constraint, _, body} ->     case convert_constraint_type(constraint) do
      {:left, err} -> {:left, err}
      {:right, c} ->
        case convert_type(body) do
          {:left, err} -> {:left, err}
          {:right, body_ty} ->
            ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_constrained(([c | []]), body_ty))
        end
    end
      {:type_parens, wrapped} ->     case convert_type(wrapped.value) do
      {:left, err} -> {:left, err}
      {:right, inner} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.ty_expr_parens(inner))
    end
      {:type_error, _} -> {:left, "Cannot convert type error"}
    end
  end



  def fold_type_ops(acc, ops) do
    case ops do
      :nil -> Nova.Runtime.pure(acc)
      ([{:tuple, op, ty} | rest]) ->         op_name = unwrap_operator(op.name)
    case convert_type(ty) do
      {:left, err} -> {:left, err}
      {:right, ty_expr} ->
                combined = Nova.Compiler.Ast.ty_expr_app((Nova.Compiler.Ast.ty_expr_app((Nova.Compiler.Ast.ty_expr_con(op_name)), acc)), ty_expr)
        fold_type_ops(combined, rest)
    end
    end
  end



  def convert_row(row) do
      Nova.Runtime.bind(case row.labels do
  :nothing -> Nova.Runtime.pure([])
  {:just, sep} -> convert_row_labels(sep)
end, fn fields ->
        tail = case row.tail do
          :nothing -> :nothing
          {:just, ({:tuple, _, ty})} -> case ty do
              {:type_var, name} -> {:just, (unwrap_ident(name.name))}
              _ -> :nothing
            end
        end
    Nova.Runtime.pure(%{fields: fields, tail: tail})
  end)
  end

  # @type labeled_label_type :: %{label: name()(label()), separator: source_token(), value: type()(void())}



  def convert_row_labels(sep) do
        all_labels = [sep.head | (Nova.Runtime.map((fn ({:tuple, _, l}) -> l end), sep.tail))]
  traverse((&convert_row_label/1), all_labels)
  end



  def convert_row_label(labeled) do
        label = unwrap_label(labeled.label.name)
  case convert_type(labeled.value) do
    {:left, err} -> {:left, err}
    {:right, ty} ->
      ((&Nova.Runtime.pure/1)).({:tuple, label, ty})
  end
  end



  def convert_expr(expr) do
    case expr do
      {:expr_hole, name} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_var((unwrap_ident(name.name))))
      {:expr_section, _} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_section("_"))
      {:expr_ident, qn} -> case qn.module do
          :nothing -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_var((unwrap_ident(qn.name))))
          {:just, mod_name} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_qualified((unwrap_module_name(mod_name)), (unwrap_ident(qn.name))))
        end
      {:expr_constructor, qn} -> case qn.module do
          :nothing -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_var((unwrap_proper(qn.name))))
          {:just, mod_name} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_qualified((unwrap_module_name(mod_name)), (unwrap_proper(qn.name))))
        end
      {:expr_boolean, _, b} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_lit((Nova.Compiler.Ast.lit_bool(b))))
      {:expr_char, _, c} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_lit((Nova.Compiler.Ast.lit_char(c))))
      {:expr_string, _, s} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_lit((Nova.Compiler.Ast.lit_string(s))))
      {:expr_int, _, int_val} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_lit((Nova.Compiler.Ast.lit_int((int_value_to_int(int_val))))))
      {:expr_number, _, n} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_lit((Nova.Compiler.Ast.lit_number(n))))
      {:expr_array, del} ->     Nova.Runtime.bind(case del.value do
  :nothing -> Nova.Runtime.pure([])
  {:just, sep} ->     all_items = [sep.head | (Nova.Runtime.map((fn ({:tuple, _, e}) -> e end), sep.tail))]
  traverse((&convert_expr/1), all_items)
end, fn items ->
      ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_list(items))
    end)
      {:expr_record, del} ->     Nova.Runtime.bind(case del.value do
  :nothing -> Nova.Runtime.pure([])
  {:just, sep} ->     all_fields = [sep.head | (Nova.Runtime.map((fn ({:tuple, _, f}) -> f end), sep.tail))]
  traverse((&convert_record_field/1), all_fields)
end, fn fields ->
      ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_record(fields))
    end)
      {:expr_parens, wrapped} ->     case convert_expr(wrapped.value) do
      {:left, err} -> {:left, err}
      {:right, inner} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_parens(inner))
    end
      {:expr_typed, e, _, ty} ->     case convert_expr(e) do
      {:left, err} -> {:left, err}
      {:right, expr_prime} ->
        case convert_type(ty) do
          {:left, err} -> {:left, err}
          {:right, ty_expr} ->
            ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_typed(expr_prime, ty_expr))
        end
    end
      {:expr_infix, head, ops} ->     case convert_expr(head) do
      {:left, err} -> {:left, err}
      {:right, head_e} ->
        fold_infix_ops(head_e, ops)
    end
      {:expr_op, head, ops} ->     case convert_expr(head) do
      {:left, err} -> {:left, err}
      {:right, head_e} ->
        fold_bin_ops(head_e, ops)
    end
      {:expr_op_name, qn} -> case qn.module do
          :nothing -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_var((unwrap_operator(qn.name))))
          {:just, mod_name} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_qualified((unwrap_module_name(mod_name)), (unwrap_operator(qn.name))))
        end
      {:expr_negate, _, e} ->     case convert_expr(e) do
      {:left, err} -> {:left, err}
      {:right, inner} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_unary_op("-", inner))
    end
      {:expr_record_accessor, acc} ->     case convert_expr(acc.expr) do
      {:left, err} -> {:left, err}
      {:right, base} ->
                path = [acc.path.head | (Nova.Runtime.map((fn ({:tuple, _, l}) -> l end), acc.path.tail))]
                labels = Nova.Runtime.map((fn l -> unwrap_label(l.name) end), path)
        case labels do
  ([first_label | rest]) ->   ((&Nova.Runtime.pure/1)).(Nova.List.foldl(fn a, b -> Nova.Compiler.Ast.expr_record_access(a, b) end, (Nova.Compiler.Ast.expr_record_access(base, first_label)), rest))
  :nil -> {:left, "Empty record accessor path"}
end
    end
      {:expr_record_update, base, updates} ->     case convert_expr(base) do
      {:left, err} -> {:left, err}
      {:right, base_e} ->
                all_updates = [updates.value.head | (Nova.Runtime.map((fn ({:tuple, _, u}) -> u end), updates.value.tail))]
        case traverse((&convert_record_update/1), all_updates) do
          {:left, err} -> {:left, err}
          {:right, update_fields} ->
            ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_record_update(base_e, update_fields))
        end
    end
      {:expr_app, head, args} ->     case convert_expr(head) do
      {:left, err} -> {:left, err}
      {:right, head_e} ->
        case traverse((&convert_expr/1), args) do
          {:left, err} -> {:left, err}
          {:right, arg_es} ->
            ((&Nova.Runtime.pure/1)).(Nova.List.foldl(fn a, b -> Nova.Compiler.Ast.expr_app(a, b) end, head_e, arg_es))
        end
    end
      {:expr_lambda, lam} ->     case traverse((&convert_binder/1), lam.binders) do
      {:left, err} -> {:left, err}
      {:right, params} ->
        case convert_expr(lam.body) do
          {:left, err} -> {:left, err}
          {:right, body} ->
            ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_lambda(params, body))
        end
    end
      {:expr_if, ite} ->     case convert_expr(ite.cond_) do
      {:left, err} -> {:left, err}
      {:right, cond_e} ->
        case convert_expr(ite.then_branch) do
          {:left, err} -> {:left, err}
          {:right, then_e} ->
            case convert_expr(ite.else_branch) do
              {:left, err} -> {:left, err}
              {:right, else_e} ->
                ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_if(cond_e, then_e, else_e))
            end
        end
    end
      {:expr_case, case_of} ->         all_heads = [case_of.head.head | (Nova.Runtime.map((fn ({:tuple, _, e}) -> e end), case_of.head.tail))]
    case all_heads do
  ([single | :nil]) -> if is_underscore(single) do
         case traverse((&convert_case_branch/1), case_of.branches) do
     {:left, err} -> {:left, err}
     {:right, clauses} ->
              lam_param = Nova.Compiler.Ast.pat_var("lamcase__")
              lam_body = Nova.Compiler.Ast.expr_case((Nova.Compiler.Ast.expr_var("lamcase__")), clauses)
       ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_lambda(([lam_param | []]), lam_body))
   end
    else
         case convert_expr(single) do
     {:left, err} -> {:left, err}
     {:right, head_e} ->
       case traverse((&convert_case_branch/1), case_of.branches) do
         {:left, err} -> {:left, err}
         {:right, clauses} ->
           ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_case(head_e, clauses))
       end
   end
    end
  multiple ->   case traverse((&convert_expr/1), multiple) do
    {:left, err} -> {:left, err}
    {:right, es} ->
      case traverse((&convert_case_branch/1), case_of.branches) do
        {:left, err} -> {:left, err}
        {:right, clauses} ->
          ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_case((Nova.Compiler.Ast.expr_tuple(es)), clauses))
      end
  end
end
      {:expr_let, let_in} ->     case traverse((&convert_let_binding/1), let_in.bindings) do
      {:left, err} -> {:left, err}
      {:right, bindings} ->
        case convert_expr(let_in.body) do
          {:left, err} -> {:left, err}
          {:right, body} ->
            ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_let(bindings, body))
        end
    end
      {:expr_do, do_block} ->     case traverse((&convert_do_statement/1), do_block.statements) do
      {:left, err} -> {:left, err}
      {:right, stmts} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_do(stmts))
    end
      {:expr_ado, ado_block} ->     case traverse((&convert_do_statement/1), ado_block.statements) do
      {:left, err} -> {:left, err}
      {:right, stmts} ->
        case convert_expr(ado_block.result) do
          {:left, err} -> {:left, err}
          {:right, result} ->
            ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.expr_do((Nova.Runtime.append(stmts, ([Nova.Compiler.Ast.do_expr(result) | []])))))
        end
    end
      {:expr_error, _} -> {:left, "Cannot convert expression error"}
    end
  end



  def fold_bin_ops(acc, ops) do
    case ops do
      :nil -> Nova.Runtime.pure(acc)
      ([{:tuple, op, e} | rest]) ->         op_name = unwrap_operator(op.name)
    case convert_expr(e) do
      {:left, err} -> {:left, err}
      {:right, expr_e} ->
                combined = Nova.Compiler.Ast.expr_bin_op(op_name, acc, expr_e)
        fold_bin_ops(combined, rest)
    end
    end
  end



  def fold_infix_ops(acc, ops) do
    case ops do
      :nil -> Nova.Runtime.pure(acc)
      ([{:tuple, wrapped, e} | rest]) ->     case convert_expr(wrapped.value) do
      {:left, err} -> {:left, err}
      {:right, fn_} ->
        case convert_expr(e) do
          {:left, err} -> {:left, err}
          {:right, expr_e} ->
                        combined = Nova.Compiler.Ast.expr_app((Nova.Compiler.Ast.expr_app(fn_, acc)), expr_e)
            fold_infix_ops(combined, rest)
        end
    end
    end
  end



  def convert_record_field(field) do
    case field do
      {:record_pun, name} ->         label = unwrap_ident(name.name)
    ((&Nova.Runtime.pure/1)).({:tuple, label, (Nova.Compiler.Ast.expr_var(label))})
      {:record_field, name, _, value} ->         label = unwrap_label(name.name)
    case convert_expr(value) do
      {:left, err} -> {:left, err}
      {:right, expr} ->
        ((&Nova.Runtime.pure/1)).({:tuple, label, expr})
    end
    end
  end



  def convert_record_update(update) do
    case update do
      {:record_update_leaf, name, _, expr} ->         label = unwrap_label(name.name)
    case convert_expr(expr) do
      {:left, err} -> {:left, err}
      {:right, e} ->
        ((&Nova.Runtime.pure/1)).({:tuple, label, e})
    end
      {:record_update_branch, name, updates} ->         label = unwrap_label(name.name)
        all_updates = [updates.value.head | (Nova.Runtime.map((fn ({:tuple, _, u}) -> u end), updates.value.tail))]
    case traverse((&convert_record_update/1), all_updates) do
      {:left, err} -> {:left, err}
      {:right, fields} ->
        ((&Nova.Runtime.pure/1)).({:tuple, label, (Nova.Compiler.Ast.expr_record(fields))})
    end
    end
  end



  def convert_case_branch(({:tuple, patterns, guarded})) do
        all_pats = [patterns.head | (Nova.Runtime.map((fn ({:tuple, _, p}) -> p end), patterns.tail))]
  Nova.Runtime.bind(case all_pats do
  ([single | :nil]) -> convert_binder(single)
  multiple ->   case traverse((&convert_binder/1), multiple) do
    {:left, err} -> {:left, err}
    {:right, ps} ->
      ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_record((list_map_with_index((fn i -> fn p -> {:tuple, (Nova.Runtime.show(i)), p} end end), ps))))
  end
end, fn pat ->
    case guarded do
  {:unconditional, _, wh} ->   case convert_expr(wh.expr) do
    {:left, err} -> {:left, err}
    {:right, body} ->
      Nova.Runtime.pure(%{pattern: pat, guard: :nothing, body: body})
  end
  {:guarded, guards} -> case guards do
      ([ge | _]) ->     case convert_pattern_guards(ge.patterns) do
      {:left, err} -> {:left, err}
      {:right, guard_clauses} ->
        case convert_expr(ge.where.expr) do
          {:left, err} -> {:left, err}
          {:right, body} ->
                        guard = case guard_clauses do
                          ([{:guard_expr, e} | _]) -> {:just, e}
                          _ -> :nothing
                        end
            Nova.Runtime.pure(%{pattern: pat, guard: guard, body: body})
        end
    end
      :nil -> {:left, "Empty guarded expression in case"}
    end
end
  end)
  end



  def convert_let_binding(binding) do
    case binding do
      {:let_binding_signature, labeled} ->         name = unwrap_ident(labeled.label.name)
    case convert_type(labeled.value) do
      {:left, err} -> {:left, err}
      {:right, ty} ->
        Nova.Runtime.pure(%{pattern: Nova.Compiler.Ast.pat_var(name), value: Nova.Compiler.Ast.expr_var(name), type_ann: {:just, ty}})
    end
      {:let_binding_name, vbf} ->         name = unwrap_ident(vbf.name.name)
    case traverse((&convert_binder/1), vbf.binders) do
      {:left, err} -> {:left, err}
      {:right, params} ->
        Nova.Runtime.bind(case vbf.guarded do
  {:unconditional, _, wh} -> convert_expr(wh.expr)
  {:guarded, guards} -> case guards do
      ([ge | _]) -> convert_expr(ge.where.expr)
      :nil -> {:left, "Empty guarded let binding"}
    end
end, fn body_expr ->
                    body = if Nova.List.null(params) do
                      body_expr
                    else
                      Nova.Compiler.Ast.expr_lambda(params, body_expr)
                    end
          Nova.Runtime.pure(%{pattern: Nova.Compiler.Ast.pat_var(name), value: body, type_ann: :nothing})
        end)
    end
      {:let_binding_pattern, binder, _, wh} ->     case convert_binder(binder) do
      {:left, err} -> {:left, err}
      {:right, pat} ->
        case convert_expr(wh.expr) do
          {:left, err} -> {:left, err}
          {:right, body} ->
            Nova.Runtime.pure(%{pattern: pat, value: body, type_ann: :nothing})
        end
    end
      {:let_binding_error, _} -> {:left, "Cannot convert let binding error"}
    end
  end



  def convert_do_statement(stmt) do
    case stmt do
      {:do_let, _, bindings} ->     case traverse((&convert_let_binding/1), bindings) do
      {:left, err} -> {:left, err}
      {:right, binds} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.do_let(binds))
    end
      {:do_discard, e} ->     case convert_expr(e) do
      {:left, err} -> {:left, err}
      {:right, expr} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.do_expr(expr))
    end
      {:do_bind, binder, _, e} ->     case convert_binder(binder) do
      {:left, err} -> {:left, err}
      {:right, pat} ->
        case convert_expr(e) do
          {:left, err} -> {:left, err}
          {:right, expr} ->
            ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.do_bind(pat, expr))
        end
    end
      {:do_error, _} -> {:left, "Cannot convert do statement error"}
    end
  end



  def convert_binder(binder) do
    case binder do
      {:binder_wildcard, _} -> Nova.Runtime.pure(Nova.Compiler.Ast.pat_wildcard())
      {:binder_var, name} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_var((unwrap_ident(name.name))))
      {:binder_named, name, _, inner} ->     case convert_binder(inner) do
      {:left, err} -> {:left, err}
      {:right, inner_pat} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_as((unwrap_ident(name.name)), inner_pat))
    end
      {:binder_constructor, qn, args} ->         name = unwrap_proper(qn.name)
    case traverse((&convert_binder/1), args) do
      {:left, err} -> {:left, err}
      {:right, arg_pats} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_con(name, arg_pats))
    end
      {:binder_boolean, _, b} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_lit((Nova.Compiler.Ast.lit_bool(b))))
      {:binder_char, _, c} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_lit((Nova.Compiler.Ast.lit_char(c))))
      {:binder_string, _, s} -> ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_lit((Nova.Compiler.Ast.lit_string(s))))
      {:binder_int, neg, _, int_val} ->         n = int_value_to_int(int_val)
        n_prime = case neg do
          :nothing -> n
          {:just, _} -> -n
        end
    ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_lit((Nova.Compiler.Ast.lit_int(n_prime))))
      {:binder_number, neg, _, num} ->         n_prime = case neg do
          :nothing -> num
          {:just, _} -> -num
        end
    ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_lit((Nova.Compiler.Ast.lit_number(n_prime))))
      {:binder_array, del} ->     Nova.Runtime.bind(case del.value do
  :nothing -> Nova.Runtime.pure([])
  {:just, sep} ->     all_items = [sep.head | (Nova.Runtime.map((fn ({:tuple, _, b}) -> b end), sep.tail))]
  traverse((&convert_binder/1), all_items)
end, fn items ->
      ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_list(items))
    end)
      {:binder_record, del} ->     Nova.Runtime.bind(case del.value do
  :nothing -> Nova.Runtime.pure([])
  {:just, sep} ->     all_fields = [sep.head | (Nova.Runtime.map((fn ({:tuple, _, f}) -> f end), sep.tail))]
  traverse((&convert_record_binder/1), all_fields)
end, fn fields ->
      ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_record(fields))
    end)
      {:binder_parens, wrapped} ->     case convert_binder(wrapped.value) do
      {:left, err} -> {:left, err}
      {:right, inner} ->
        ((&Nova.Runtime.pure/1)).(Nova.Compiler.Ast.pat_parens(inner))
    end
      {:binder_typed, inner, _, _} ->     convert_binder(inner)
      {:binder_op, head, ops} ->     case convert_binder(head) do
      {:left, err} -> {:left, err}
      {:right, head_pat} ->
        fold_binder_ops(head_pat, ops)
    end
      {:binder_error, _} -> {:left, "Cannot convert binder error"}
    end
  end



  def fold_binder_ops(acc, ops) do
    case ops do
      :nil -> Nova.Runtime.pure(acc)
      ([{:tuple, op, b} | rest]) ->         op_name = unwrap_operator(op.name)
    case convert_binder(b) do
      {:left, err} -> {:left, err}
      {:right, binder_pat} ->
                combined = if (op_name == ":") do
                  Nova.Compiler.Ast.pat_cons(acc, binder_pat)
                else
                  Nova.Compiler.Ast.pat_con(op_name, ([[acc | binder_pat] | []]))
                end
        fold_binder_ops(combined, rest)
    end
    end
  end



  def convert_record_binder(field) do
    case field do
      {:record_pun, name} ->         label = unwrap_ident(name.name)
    ((&Nova.Runtime.pure/1)).({:tuple, label, (Nova.Compiler.Ast.pat_var(label))})
      {:record_field, name, _, binder} ->         label = unwrap_label(name.name)
    case convert_binder(binder) do
      {:left, err} -> {:left, err}
      {:right, pat} ->
        ((&Nova.Runtime.pure/1)).({:tuple, label, pat})
    end
    end
  end
end
