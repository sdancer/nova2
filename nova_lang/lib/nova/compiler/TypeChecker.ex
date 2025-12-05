defmodule Nova.Compiler.TypeChecker do
  # import Prelude

  # import Data.Either

  # import Data.Maybe

  # import Data.Tuple

  # import Data.Array

  # import Data.Array

  # import Data.List

  # import Data.List

  # import Data.Foldable

  # import Data.Map

  # import Data.Set

  # import Data.String

  # import Nova.Compiler.Types

  # import Nova.Compiler.Types

  # import Nova.Compiler.Ast

  # import Nova.Compiler.Unify

  # Data type: TCError
  def unify_err(arg0), do: {:unify_err, arg0}
  def unbound_variable(arg0), do: {:unbound_variable, arg0}
  def not_implemented(arg0), do: {:not_implemented, arg0}

  # instance Show tcerror()



  def list_map_with_index(f, list) do
    
      go = Nova.Runtime.fix2(fn go -> fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {_, []} -> []
        {i, ([x | xs])} -> [(f.(i).(x)) | (go.(((i + 1))).(xs))]
      end end end end)
      go.(0).(list)
  end



  def instantiate_go(scheme_ty, e, [], sub) do
    %{ty: Nova.Compiler.Types.apply_subst(sub, scheme_ty), env: e}
  end

  def instantiate_go(scheme_ty, e, ([v | rest]), sub) do
    
      {:tuple, fresh, e_prime} = Nova.Compiler.Types.fresh_var(e, v.name)
      sub_prime = Nova.Map.insert(v.id, ({:ty_var, fresh}), sub)
      instantiate_go(scheme_ty, e_prime, rest, sub_prime)
  end



  def instantiate(env, scheme) do
    instantiate_go(scheme.ty, env, (Nova.List.from_foldable(scheme.vars)), Nova.Map.empty)
  end



  def generalize(env, ty) do
    
      env_free = Nova.Compiler.Types.free_type_vars_env(env)
      ty_free = Nova.Compiler.Types.free_type_vars(ty)
      free_ids = Nova.Set.to_unfoldable((Nova.Set.difference(ty_free, env_free)))
      vars = Nova.Runtime.map((fn i -> Nova.Compiler.Types.mk_tvar(i, (Nova.Runtime.append("t", Nova.Runtime.show(i)))) end), free_ids)
      Nova.Compiler.Types.mk_scheme(vars, ty)
  end



  def infer_lit(({:lit_int, _})) do
    Nova.Compiler.Types.t_int()
  end

  def infer_lit(({:lit_number, _})) do
    {:ty_con, (Nova.Compiler.Types.mk_tcon("Number", []))}
  end

  def infer_lit(({:lit_string, _})) do
    Nova.Compiler.Types.t_string()
  end

  def infer_lit(({:lit_char, _})) do
    Nova.Compiler.Types.t_char()
  end

  def infer_lit(({:lit_bool, _})) do
    Nova.Compiler.Types.t_bool()
  end

  # @type infer_result :: %{ty: type(), sub: subst(), env: env()}



  def infer(env, ({:expr_lit, lit})) do
    {:right, %{ty: infer_lit(lit), sub: Nova.Compiler.Types.empty_subst(), env: env}}
  end

  def infer(env, ({:expr_var, name})) do
    case Nova.Compiler.Types.lookup_env(env, name) do
      :nothing -> {:left, ({:unbound_variable, name})}
      {:just, scheme} -> 
          r = instantiate(env, scheme)
          {:right, %{ty: r.ty, sub: Nova.Compiler.Types.empty_subst(), env: r.env}}
    end
  end

  def infer(env, ({:expr_qualified, m, name})) do
    
      full_name = Nova.Runtime.append(Nova.Runtime.append(m, "."), name)
      case Nova.Compiler.Types.lookup_env(env, full_name) do
  {:just, scheme} -> 
      r = instantiate(env, scheme)
      {:right, %{ty: r.ty, sub: Nova.Compiler.Types.empty_subst(), env: r.env}}
  :nothing -> case Nova.Compiler.Types.lookup_env(env, name) do
      {:just, scheme} -> 
          r = instantiate(env, scheme)
          {:right, %{ty: r.ty, sub: Nova.Compiler.Types.empty_subst(), env: r.env}}
      :nothing -> {:left, ({:unbound_variable, full_name})}
    end
end
  end

  def infer(env, ({:expr_app, f, arg})) do
    
      infer_app = fn e -> fn func -> fn a -> case infer(e, func) do
        {:left, err} -> {:left, err}
        {:right, r1} -> case infer(r1.env, a) do
            {:left, err} -> {:left, err}
            {:right, r2} -> 
                {:tuple, tv, env3} = Nova.Compiler.Types.fresh_var(r2.env, "r")
                result_ty = {:ty_var, tv}
                case Nova.Compiler.Unify.unify((Nova.Compiler.Types.apply_subst(r2.sub, r1.ty)), (Nova.Compiler.Types.t_arrow(r2.ty, result_ty))) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s3} -> 
      sub = Nova.Compiler.Types.compose_subst(s3, (Nova.Compiler.Types.compose_subst(r2.sub, r1.sub)))
      {:right, %{ty: Nova.Compiler.Types.apply_subst(s3, result_ty), sub: sub, env: env3}}
end
          end
      end end end end
      case f do
  {:expr_var, "-"} -> case arg do
      {:expr_lit, ({:lit_int, _})} -> {:right, %{ty: Nova.Compiler.Types.t_int(), sub: Nova.Compiler.Types.empty_subst(), env: env}}
      {:expr_lit, ({:lit_number, _})} -> {:right, %{ty: {:ty_con, (Nova.Compiler.Types.mk_tcon("Number", []))}, sub: Nova.Compiler.Types.empty_subst(), env: env}}
      {:expr_parens, ({:expr_lit, ({:lit_int, _})})} -> {:right, %{ty: Nova.Compiler.Types.t_int(), sub: Nova.Compiler.Types.empty_subst(), env: env}}
      _ -> infer_app.(env).(f).(arg)
    end
  _ -> infer_app.(env).(f).(arg)
end
  end

  def infer(env, ({:expr_lambda, pats, body})) do
    case Nova.List.uncons(pats) do
      :nothing -> infer(env, body)
      {:just, %{head: pat, tail: rest_pats}} -> 
          {:tuple, arg_tv, env1} = Nova.Compiler.Types.fresh_var(env, "a")
          arg_ty = {:ty_var, arg_tv}
          case infer_pat(env1, pat, arg_ty) do
  {:left, e} -> {:left, e}
  {:right, pat_res} -> 
      inner_expr = if Nova.List.null(rest_pats) do
        body
      else
        Nova.Compiler.Ast.expr_lambda(rest_pats, body)
      end
      case infer(pat_res.env, inner_expr) do
  {:left, e} -> {:left, e}
  {:right, body_res} -> 
      sub = Nova.Compiler.Types.compose_subst(body_res.sub, pat_res.sub)
      result_ty = Nova.Compiler.Types.t_arrow((Nova.Compiler.Types.apply_subst(sub, arg_ty)), body_res.ty)
      {:right, %{ty: result_ty, sub: sub, env: body_res.env}}
end
end
    end
  end

  def infer(env, ({:expr_let, binds, body})) do
    case infer_binds(env, binds) do
      {:left, e} -> {:left, e}
      {:right, let_res} -> case infer(let_res.env, body) do
          {:left, e} -> {:left, e}
          {:right, body_res} -> {:right, %{ty: body_res.ty, sub: Nova.Compiler.Types.compose_subst(body_res.sub, let_res.sub), env: body_res.env}}
        end
    end
  end

  def infer(env, ({:expr_if, cond_, then_, else_})) do
    case infer(env, cond_) do
      {:left, e} -> {:left, e}
      {:right, cond_res} -> case Nova.Compiler.Unify.unify(cond_res.ty, Nova.Compiler.Types.t_bool()) do
          {:left, ue} -> {:left, ({:unify_err, ue})}
          {:right, _} -> case infer(cond_res.env, then_) do
              {:left, e} -> {:left, e}
              {:right, then_res} -> case infer(then_res.env, else_) do
                  {:left, e} -> {:left, e}
                  {:right, else_res} -> case Nova.Compiler.Unify.unify((Nova.Compiler.Types.apply_subst(else_res.sub, then_res.ty)), else_res.ty) do
                      {:left, ue} -> {:left, ({:unify_err, ue})}
                      {:right, s} -> {:right, %{ty: Nova.Compiler.Types.apply_subst(s, else_res.ty), sub: Nova.Compiler.Types.compose_subst(s, else_res.sub), env: else_res.env}}
                    end
                end
            end
        end
    end
  end

  def infer(env, ({:expr_case, scrutinee, clauses})) do
    case infer(env, scrutinee) do
      {:left, e} -> {:left, e}
      {:right, scrut_res} -> 
          {:tuple, result_tv, env2} = Nova.Compiler.Types.fresh_var(scrut_res.env, "case")
          result_ty = {:ty_var, result_tv}
          case infer_clauses(env2, scrut_res.ty, result_ty, clauses, scrut_res.sub) do
  {:left, e} -> {:left, e}
  {:right, clause_res} -> {:right, %{ty: Nova.Compiler.Types.apply_subst(clause_res.sub, result_ty), sub: clause_res.sub, env: clause_res.env}}
end
    end
  end

  def infer(env, ({:expr_bin_op, op, l, r})) do
    case Nova.Compiler.Types.lookup_env(env, op) do
      :nothing -> {:left, ({:unbound_variable, op})}
      {:just, scheme} -> 
          op_inst = instantiate(env, scheme)
          case infer(op_inst.env, l) do
  {:left, e} -> {:left, e}
  {:right, l_res} -> case infer(l_res.env, r) do
      {:left, e} -> {:left, e}
      {:right, r_res} -> 
          {:tuple, res_tv, env4} = Nova.Compiler.Types.fresh_var(r_res.env, "binop")
          case Nova.Compiler.Unify.unify((Nova.Compiler.Types.apply_subst((Nova.Compiler.Types.compose_subst(r_res.sub, l_res.sub)), op_inst.ty)), (Nova.Compiler.Types.t_arrow(l_res.ty, (Nova.Compiler.Types.t_arrow(r_res.ty, ({:ty_var, res_tv})))))) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s4} -> 
      sub = Nova.Compiler.Types.compose_subst(s4, (Nova.Compiler.Types.compose_subst(r_res.sub, l_res.sub)))
      {:right, %{ty: Nova.Compiler.Types.apply_subst(s4, ({:ty_var, res_tv})), sub: sub, env: env4}}
end
    end
end
    end
  end

  def infer(env, ({:expr_list, elems})) do
    
      {:tuple, elem_tv, env1} = Nova.Compiler.Types.fresh_var(env, "elem")
      elem_ty = {:ty_var, elem_tv}
      case infer_elems(env1, elem_ty, elems) do
  {:left, e} -> {:left, e}
  {:right, res} -> {:right, %{ty: {:ty_con, (Nova.Compiler.Types.mk_tcon("Array", [Nova.Compiler.Types.apply_subst(res.sub, elem_ty)]))}, sub: res.sub, env: res.env}}
end
  end

  def infer(env, ({:expr_tuple, elems})) do
    case infer_many(env, elems) do
      {:left, e} -> {:left, e}
      {:right, res} -> {:right, %{ty: Nova.Compiler.Types.t_tuple(res.tys), sub: res.sub, env: res.env}}
    end
  end

  def infer(env, ({:expr_record, fields})) do
    case infer_fields(env, fields) do
      {:left, e} -> {:left, e}
      {:right, res} -> {:right, %{ty: {:ty_record, %{fields: Nova.Map.from_foldable(res.tys), row: :nothing}}, sub: res.sub, env: res.env}}
    end
  end

  def infer(env, ({:expr_record_access, rec, field})) do
    case infer(env, rec) do
      {:left, e} -> {:left, e}
      {:right, rec_res} -> 
          {:tuple, result_tv, env2} = Nova.Compiler.Types.fresh_var(rec_res.env, "field")
          {:tuple, row_tv, env3} = Nova.Compiler.Types.fresh_var(env2, "row")
          expected_rec = {:ty_record, %{fields: Nova.Map.singleton(field, ({:ty_var, result_tv})), row: {:just, row_tv}}}
          case Nova.Compiler.Unify.unify(rec_res.ty, expected_rec) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s2} -> 
      sub = Nova.Compiler.Types.compose_subst(s2, rec_res.sub)
      {:right, %{ty: Nova.Compiler.Types.apply_subst(sub, ({:ty_var, result_tv})), sub: sub, env: env3}}
end
    end
  end

  def infer(env, ({:expr_parens, e})) do
    infer(env, e)
  end

  def infer(env, ({:expr_typed, e, _})) do
    infer(env, e)
  end

  def infer(env, ({:expr_record_update, rec, updates})) do
    infer_record_update(env, rec, (Nova.Array.from_foldable(updates)))
  end

  def infer(env, ({:expr_unary_op, op, e})) do
    infer_unary_op(env, op, e)
  end

  def infer(env, ({:expr_do, stmts})) do
    infer_do(env, (Nova.Array.from_foldable(stmts)))
  end

  def infer(_, _) do
    {:left, ({:not_implemented, "expression form"})}
  end



  def infer_record_update(env, rec, updates) do
      case infer(env, rec) do
    {:left, err} -> {:left, err}
    {:right, rec_res} ->
      case infer_fields(rec_res.env, (Nova.List.from_foldable(updates))) do
        {:left, err} -> {:left, err}
        {:right, update_res} ->
                    {:tuple, row_var, env2} = Nova.Compiler.Types.fresh_var(update_res.env, "row")
                    update_fields = Nova.Map.from_foldable(update_res.tys)
                    expected_rec = {:ty_record, %{fields: update_fields, row: {:just, row_var}}}
          case Nova.Compiler.Unify.unify((Nova.Compiler.Types.apply_subst(update_res.sub, rec_res.ty)), expected_rec) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s} -> 
      sub = Nova.Compiler.Types.compose_subst(s, (Nova.Compiler.Types.compose_subst(update_res.sub, rec_res.sub)))
      {:right, %{ty: Nova.Compiler.Types.apply_subst(sub, rec_res.ty), sub: sub, env: env2}}
end
      end
  end
  end



  def infer_unary_op(env, op, e) do
    case op do
      "-" ->     case infer(env, e) do
      {:left, err} -> {:left, err}
      {:right, res} ->
        case Nova.Compiler.Unify.unify(res.ty, Nova.Compiler.Types.t_int()) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s} -> {:right, %{ty: Nova.Compiler.Types.t_int(), sub: Nova.Compiler.Types.compose_subst(s, res.sub), env: res.env}}
end
    end
      "!" ->     case infer(env, e) do
      {:left, err} -> {:left, err}
      {:right, res} ->
        case Nova.Compiler.Unify.unify(res.ty, Nova.Compiler.Types.t_bool()) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s} -> {:right, %{ty: Nova.Compiler.Types.t_bool(), sub: Nova.Compiler.Types.compose_subst(s, res.sub), env: res.env}}
end
    end
      _ -> case Nova.Compiler.Types.lookup_env(env, op) do
          :nothing -> {:left, ({:unbound_variable, op})}
          {:just, scheme} ->             op_inst = instantiate(env, scheme)
      case infer(op_inst.env, e) do
        {:left, err} -> {:left, err}
        {:right, res} ->
                    {:tuple, res_tv, env2} = Nova.Compiler.Types.fresh_var(res.env, "unary")
          case Nova.Compiler.Unify.unify(op_inst.ty, (Nova.Compiler.Types.t_arrow(res.ty, ({:ty_var, res_tv})))) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s} -> 
      sub = Nova.Compiler.Types.compose_subst(s, res.sub)
      {:right, %{ty: Nova.Compiler.Types.apply_subst(s, ({:ty_var, res_tv})), sub: sub, env: env2}}
end
      end
        end
    end
  end



  def infer_do(env, stmts) do
    case Nova.Array.uncons(stmts) do
      :nothing -> {:left, ({:not_implemented, "empty do block"})}
      {:just, %{head: stmt, tail: rest}} -> case stmt do
          {:do_expr, e} -> if Nova.Array.null(rest) do
              infer(env, e)
            else
                     case infer(env, e) do
         {:left, err} -> {:left, err}
         {:right, res1} ->
           case infer_do(res1.env, rest) do
             {:left, err} -> {:left, err}
             {:right, rest_res} ->
               {:right, %{ty: rest_res.ty, sub: Nova.Compiler.Types.compose_subst(rest_res.sub, res1.sub), env: rest_res.env}}
           end
       end
            end
          {:do_bind, pat, e} ->       case infer(env, e) do
        {:left, err} -> {:left, err}
        {:right, res1} ->
                    {:tuple, inner_tv, env1} = Nova.Compiler.Types.fresh_var(res1.env, "inner")
                    inner_ty = {:ty_var, inner_tv}
          case infer_pat(env1, pat, inner_ty) do
            {:left, err} -> {:left, err}
            {:right, pat_res} ->
              case infer_do(pat_res.env, rest) do
                {:left, err} -> {:left, err}
                {:right, rest_res} ->
                                    sub = Nova.Compiler.Types.compose_subst(rest_res.sub, (Nova.Compiler.Types.compose_subst(pat_res.sub, res1.sub)))
                  {:right, %{ty: rest_res.ty, sub: sub, env: rest_res.env}}
              end
          end
      end
          {:do_let, binds} ->       case infer_binds(env, binds) do
        {:left, err} -> {:left, err}
        {:right, let_res} ->
          infer_do(let_res.env, rest)
      end
        end
    end
  end

  # @type many_result :: %{tys: array()(type()), sub: subst(), env: env()}



  def infer_many_go(e, [], acc, sub) do
    {:right, %{tys: Nova.Array.from_foldable((Nova.List.reverse(acc))), sub: sub, env: e}}
  end

  def infer_many_go(e, ([expr | rest]), acc, sub) do
    case infer(e, expr) do
      {:left, err} -> {:left, err}
      {:right, res} -> infer_many_go(res.env, rest, ([res.ty | acc]), (Nova.Compiler.Types.compose_subst(res.sub, sub)))
    end
  end



  def infer_many(env, exprs) do
    infer_many_go(env, exprs, [], Nova.Compiler.Types.empty_subst())
  end

  # @type elems_result :: %{sub: subst(), env: env()}



  def infer_elems_go(e, _, [], sub) do
    {:right, %{sub: sub, env: e}}
  end

  def infer_elems_go(e, e_ty, ([expr | rest]), sub) do
    case infer(e, expr) do
      {:left, err} -> {:left, err}
      {:right, res} -> case Nova.Compiler.Unify.unify((Nova.Compiler.Types.apply_subst(res.sub, e_ty)), res.ty) do
          {:left, ue} -> {:left, ({:unify_err, ue})}
          {:right, s2} -> infer_elems_go(res.env, (Nova.Compiler.Types.apply_subst(s2, e_ty)), rest, (Nova.Compiler.Types.compose_subst(s2, (Nova.Compiler.Types.compose_subst(res.sub, sub)))))
        end
    end
  end



  def infer_elems(env, elem_ty, elems) do
    infer_elems_go(env, elem_ty, elems, Nova.Compiler.Types.empty_subst())
  end

  # @type fields_result :: %{tys: list()((tuple()(string())(type()))), sub: subst(), env: env()}



  def infer_fields_go(e, [], acc, sub) do
    {:right, %{tys: Nova.List.reverse(acc), sub: sub, env: e}}
  end

  def infer_fields_go(e, ([({:tuple, name, expr}) | rest]), acc, sub) do
    case infer(e, expr) do
      {:left, err} -> {:left, err}
      {:right, res} -> infer_fields_go(res.env, rest, ([({:tuple, name, res.ty}) | acc]), (Nova.Compiler.Types.compose_subst(res.sub, sub)))
    end
  end



  def infer_fields(env, fields) do
    infer_fields_go(env, fields, [], Nova.Compiler.Types.empty_subst())
  end

  # @type pat_result :: %{env: env(), sub: subst()}



  def infer_pat(env, ({:pat_var, name}), ty) do
    
      scheme = Nova.Compiler.Types.mk_scheme([], ty)
      env_prime = Nova.Compiler.Types.extend_env(env, name, scheme)
      {:right, %{env: env_prime, sub: Nova.Compiler.Types.empty_subst()}}
  end

  def infer_pat(env, :pat_wildcard, _) do
    {:right, %{env: env, sub: Nova.Compiler.Types.empty_subst()}}
  end

  def infer_pat(env, ({:pat_lit, lit}), ty) do
    
      lit_ty = infer_lit(lit)
      case Nova.Compiler.Unify.unify(ty, lit_ty) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s} -> {:right, %{env: env, sub: s}}
end
  end

  def infer_pat(env, ({:pat_con, con_name, pats}), ty) do
    
      unqualified_name = case Nova.String.last_index_of((Nova.String.pattern(".")), con_name) do
        {:just, idx} -> Nova.String.drop(((idx + 1)), con_name)
        :nothing -> con_name
      end
      try_lookup = fn name -> Nova.Compiler.Types.lookup_env(env, name) end
      case try_lookup.(con_name) do
  {:just, scheme} -> 
      r = instantiate(env, scheme)
      infer_con_pats(r.env, r.ty, pats, ty)
  :nothing -> case try_lookup.(unqualified_name) do
      {:just, scheme} -> 
          r = instantiate(env, scheme)
          infer_con_pats(r.env, r.ty, pats, ty)
      :nothing -> {:left, ({:unbound_variable, con_name})}
    end
end
  end

  def infer_pat(env, ({:pat_parens, p}), ty) do
    infer_pat(env, p, ty)
  end

  def infer_pat(env, ({:pat_record, fields}), ty) do
    infer_record_pat(env, fields, ty)
  end

  def infer_pat(env, ({:pat_list, pats}), ty) do
    infer_list_pat(env, pats, ty)
  end

  def infer_pat(env, ({:pat_cons, hd, tl}), ty) do
    infer_cons_pat(env, hd, tl, ty)
  end

  def infer_pat(env, ({:pat_as, name, pat}), ty) do
      case infer_pat(env, pat, ty) do
    {:left, err} -> {:left, err}
    {:right, pat_res} ->
            scheme = Nova.Compiler.Types.mk_scheme([], (Nova.Compiler.Types.apply_subst(pat_res.sub, ty)))
            env_prime = Nova.Compiler.Types.extend_env(pat_res.env, name, scheme)
      {:right, %{env: env_prime, sub: pat_res.sub}}
  end
  end

  def infer_pat(_, _, _) do
    {:left, ({:not_implemented, "pattern form"})}
  end



  def infer_record_pat_go(ty, e, [], field_types, sub) do
        {:tuple, row_var, e_prime} = Nova.Compiler.Types.fresh_var(e, "row")
    expected_rec = {:ty_record, %{fields: field_types, row: {:just, row_var}}}
  case Nova.Compiler.Unify.unify((Nova.Compiler.Types.apply_subst(sub, ty)), expected_rec) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s} -> {:right, %{env: e_prime, sub: Nova.Compiler.Types.compose_subst(s, sub)}}
end
  end

  def infer_record_pat_go(ty, e, ([({:tuple, label, pat}) | rest]), field_types, sub) do
        {:tuple, field_var, e1} = Nova.Compiler.Types.fresh_var(e, (Nova.Runtime.append("f_", label)))
    field_ty = {:ty_var, field_var}
  case infer_pat(e1, pat, field_ty) do
    {:left, err} -> {:left, err}
    {:right, pat_res} ->
      infer_record_pat_go(ty, pat_res.env, rest, (Nova.Map.insert(label, (Nova.Compiler.Types.apply_subst(pat_res.sub, field_ty)), field_types)), (Nova.Compiler.Types.compose_subst(pat_res.sub, sub)))
  end
  end



  def infer_record_pat(env, fields, ty) do
    infer_record_pat_go(ty, env, fields, Nova.Map.empty, Nova.Compiler.Types.empty_subst())
  end



  def infer_list_pat(env, pats, ty) do
    
      go_elems = Nova.Runtime.fix4(fn go_elems -> fn auto_arg0 -> fn auto_arg1 -> fn auto_arg2 -> fn auto_arg3 -> case {auto_arg0, auto_arg1, auto_arg2, auto_arg3} do
        {e, [], _, sub} -> {:right, %{env: e, sub: sub}}
        {e, ([p | rest]), e_ty, sub} -> case infer_pat(e, p, e_ty) do
  {:left, err} -> {:left, err}
  {:right, pat_res} ->
    go_elems.(pat_res.env).(rest).((Nova.Compiler.Types.apply_subst(pat_res.sub, e_ty))).((Nova.Compiler.Types.compose_subst(pat_res.sub, sub)))
end
      end end end end end end)
      {:tuple, elem_var, env1} = Nova.Compiler.Types.fresh_var(env, "elem")
elem_ty = {:ty_var, elem_var}
case Nova.Compiler.Unify.unify(ty, (Nova.Compiler.Types.t_array(elem_ty))) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s1} ->     elem_ty_prime = Nova.Compiler.Types.apply_subst(s1, elem_ty)
  go_elems.(env1).(pats).(elem_ty_prime).(s1)
end
  end



  def infer_cons_pat(env, hd_pat, tl_pat, ty) do
        {:tuple, elem_var, env1} = Nova.Compiler.Types.fresh_var(env, "elem")
    elem_ty = {:ty_var, elem_var}
  case Nova.Compiler.Unify.unify(ty, (Nova.Compiler.Types.t_array(elem_ty))) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s1} ->     elem_ty_prime = Nova.Compiler.Types.apply_subst(s1, elem_ty)
    list_ty = Nova.Compiler.Types.apply_subst(s1, (Nova.Compiler.Types.t_array(elem_ty)))
  case infer_pat(env1, hd_pat, elem_ty_prime) do
    {:left, err} -> {:left, err}
    {:right, hd_res} ->
      case infer_pat(hd_res.env, tl_pat, (Nova.Compiler.Types.apply_subst(hd_res.sub, list_ty))) do
        {:left, err} -> {:left, err}
        {:right, tl_res} ->
          {:right, %{env: tl_res.env, sub: Nova.Compiler.Types.compose_subst(tl_res.sub, (Nova.Compiler.Types.compose_subst(hd_res.sub, s1)))}}
      end
  end
end
  end



  def infer_con_pats_go(result_ty, e, ty, [], sub) do
    case Nova.Compiler.Unify.unify(ty, result_ty) do
      {:left, ue} -> {:left, ({:unify_err, ue})}
      {:right, s} -> {:right, %{env: e, sub: Nova.Compiler.Types.compose_subst(s, sub)}}
    end
  end

  def infer_con_pats_go(result_ty, e, ty, ([p | rest]), sub) do
    case ty do
      {:ty_con, c} ->
        cond do
          (c.name == "Fun") -> case %{a: Nova.Array.head(c.args), b: Nova.Array.last(c.args)} do
              %{a: {:just, arg_ty}, b: {:just, res_ty}} -> case infer_pat(e, p, arg_ty) do
                  {:left, err} -> {:left, err}
                  {:right, pat_res} -> infer_con_pats_go(result_ty, pat_res.env, res_ty, rest, (Nova.Compiler.Types.compose_subst(pat_res.sub, sub)))
                end
              _ -> {:left, ({:not_implemented, "malformed function type"})}
            end
          true -> {:left, ({:not_implemented, "expected function type in constructor"})}
        end
      _ -> {:left, ({:not_implemented, "expected function type in constructor"})}
    end
  end



  def infer_con_pats(env, con_ty, pats, result_ty) do
    infer_con_pats_go(result_ty, env, con_ty, pats, Nova.Compiler.Types.empty_subst())
  end



  def infer_binds(env, binds) do
    
      add_one = fn e -> fn bind -> case bind.pattern do
        {:pat_var, name} -> 
            {:tuple, tv, e_prime} = Nova.Compiler.Types.fresh_var(e, (Nova.Runtime.append("let_", name)))
            Nova.Compiler.Types.extend_env(e_prime, name, (Nova.Compiler.Types.mk_scheme([], ({:ty_var, tv}))))
        _ -> e
      end end end
      infer_binds_pass2 = Nova.Runtime.fix3(fn infer_binds_pass2 -> fn auto_arg0 -> fn auto_arg1 -> fn auto_arg2 -> case {auto_arg0, auto_arg1, auto_arg2} do
        {e, [], sub} -> {:right, %{env: e, sub: sub}}
        {e, ([bind | rest]), sub} -> case infer(e, bind.value) do
  {:left, err} -> {:left, err}
  {:right, val_res} -> case infer_pat(val_res.env, bind.pattern, val_res.ty) do
      {:left, err} -> {:left, err}
      {:right, pat_res} -> 
          scheme = generalize(e, (Nova.Compiler.Types.apply_subst((Nova.Compiler.Types.compose_subst(pat_res.sub, val_res.sub)), val_res.ty)))
          env3 = case bind.pattern do
            {:pat_var, name} -> Nova.Compiler.Types.extend_env((%{e | counter: pat_res.env.counter}), name, scheme)
            _ -> 
                pat_env = Nova.Compiler.Types.apply_subst_to_env((Nova.Compiler.Types.compose_subst(pat_res.sub, val_res.sub)), pat_res.env)
                %{pat_env | bindings: Nova.Map.union(pat_env.bindings, e.bindings)}
          end
          infer_binds_pass2.(env3).(rest).((Nova.Compiler.Types.compose_subst(pat_res.sub, (Nova.Compiler.Types.compose_subst(val_res.sub, sub)))))
    end
end
      end end end end end)
      add_bind_placeholders = fn e -> fn bs -> Nova.Runtime.foldl(add_one, e, bs) end end
      
  env_with_placeholders = add_bind_placeholders.(env).(binds)
  infer_binds_pass2.(env_with_placeholders).(binds).(Nova.Compiler.Types.empty_subst())
  end



  def expr_to_pattern(({:expr_var, name})) do
    Nova.Compiler.Ast.pat_var(name)
  end

  def expr_to_pattern(({:expr_app, ({:expr_var, con}), arg})) do
    Nova.Compiler.Ast.pat_con(con, ([(expr_to_pattern(arg)) | []]))
  end

  def expr_to_pattern(({:expr_app, ({:expr_app, ({:expr_var, con}), arg1}), arg2})) do
    Nova.Compiler.Ast.pat_con(con, ([(expr_to_pattern(arg1)) | ([(expr_to_pattern(arg2)) | []])]))
  end

  def expr_to_pattern(({:expr_lit, lit})) do
    Nova.Compiler.Ast.pat_lit(lit)
  end

  def expr_to_pattern(({:expr_parens, e})) do
    expr_to_pattern(e)
  end

  def expr_to_pattern(_) do
    :pat_wildcard
  end



  def infer_guard_expr(e, ({:expr_bin_op, "&&", left, right})) do
    case infer_guard_expr(e, left) do
      {:left, err} -> {:left, err}
      {:right, left_res} -> case infer_guard_expr(left_res.env, right) do
          {:left, err} -> {:left, err}
          {:right, right_res} -> {:right, %{env: right_res.env, sub: Nova.Compiler.Types.compose_subst(right_res.sub, left_res.sub)}}
        end
    end
  end

  def infer_guard_expr(e, ({:expr_bin_op, "<-", pat_expr, val_expr})) do
    case infer(e, val_expr) do
      {:left, err} -> {:left, err}
      {:right, val_res} -> 
          pat = expr_to_pattern(pat_expr)
          case infer_pat(val_res.env, pat, val_res.ty) do
  {:left, err} -> {:left, err}
  {:right, pat_res} -> {:right, %{env: pat_res.env, sub: Nova.Compiler.Types.compose_subst(pat_res.sub, val_res.sub)}}
end
    end
  end

  def infer_guard_expr(e, ({:expr_bin_op, ",", left, right})) do
    infer_guard_expr(e, (Nova.Compiler.Ast.expr_bin_op("&&", left, right)))
  end

  def infer_guard_expr(e, expr) do
    case infer(e, expr) do
      {:left, err} -> {:left, err}
      {:right, res} -> {:right, %{env: res.env, sub: res.sub}}
    end
  end



  def infer_guard(e, :nothing) do
    {:right, %{env: e, sub: Nova.Compiler.Types.empty_subst()}}
  end

  def infer_guard(e, ({:just, guard_expr})) do
    infer_guard_expr(e, guard_expr)
  end



  def infer_clauses_go(_, _, e, [], sub) do
    {:right, %{env: e, sub: sub}}
  end

  def infer_clauses_go(s_ty, r_ty, e, ([clause | rest]), sub) do
    
      s_ty_prime = Nova.Compiler.Types.apply_subst(sub, s_ty)
      r_ty_prime = Nova.Compiler.Types.apply_subst(sub, r_ty)
      case infer_pat(e, clause.pattern, s_ty_prime) do
  {:left, err} -> {:left, err}
  {:right, pat_res} -> 
      pat_env = Nova.Compiler.Types.apply_subst_to_env(pat_res.sub, pat_res.env)
      case infer_guard(pat_env, clause.guard) do
  {:left, err} -> {:left, err}
  {:right, guard_res} -> case infer(guard_res.env, clause.body) do
      {:left, err} -> {:left, err}
      {:right, body_res} -> case Nova.Compiler.Unify.unify((Nova.Compiler.Types.apply_subst(body_res.sub, r_ty_prime)), body_res.ty) do
          {:left, ue} -> {:left, ({:unify_err, ue})}
          {:right, s} -> 
              new_sub = Nova.Compiler.Types.compose_subst(s, (Nova.Compiler.Types.compose_subst(body_res.sub, (Nova.Compiler.Types.compose_subst(guard_res.sub, (Nova.Compiler.Types.compose_subst(pat_res.sub, sub)))))))
              e_prime = %{e | counter: body_res.env.counter}
              infer_clauses_go(s_ty, r_ty, e_prime, rest, new_sub)
        end
    end
end
end
  end



  def infer_clauses(env, scrut_ty, result_ty, clauses, init_sub) do
    infer_clauses_go(scrut_ty, result_ty, env, clauses, init_sub)
  end



  def check_function(env, func) do
    
      {:tuple, func_tv, env1} = Nova.Compiler.Types.fresh_var(env, (Nova.Runtime.append("fn_", func.name)))
      func_ty = {:ty_var, func_tv}
      expr = case func.parameters do
        [] -> func.body
        _ -> Nova.Compiler.Ast.expr_lambda(func.parameters, func.body)
      end
      temp_scheme = Nova.Compiler.Types.mk_scheme([], func_ty)
      env_with_func = Nova.Compiler.Types.extend_env(env1, func.name, temp_scheme)
      case infer(env_with_func, expr) do
  {:left, e} -> {:left, e}
  {:right, res} ->   case Nova.Compiler.Unify.unify((Nova.Compiler.Types.apply_subst(res.sub, func_ty)), res.ty) do
  {:left, ue} -> {:left, ({:unify_err, ue})}
  {:right, s} -> 
      final_sub = Nova.Compiler.Types.compose_subst(s, res.sub)
      final_ty = Nova.Compiler.Types.apply_subst(final_sub, res.ty)
      scheme = generalize(res.env, final_ty)
      {:right, %{scheme: scheme, env: Nova.Compiler.Types.extend_env(res.env, func.name, scheme)}}
end
end
  end



  def check_decl(env, ({:decl_function, func})) do
    case check_function(env, func) do
      {:left, e} -> {:left, e}
      {:right, r} -> {:right, r.env}
    end
  end

  def check_decl(env, ({:decl_type_sig, _})) do
    {:right, env}
  end

  def check_decl(env, ({:decl_data_type, dt})) do
    {:right, (check_data_type(env, dt))}
  end

  def check_decl(env, ({:decl_type_alias, ta})) do
    {:right, (check_type_alias(env, ta))}
  end

  def check_decl(env, ({:decl_type_class, tc})) do
    {:right, (check_type_class(env, tc))}
  end

  def check_decl(env, _) do
    {:right, env}
  end



  def check_type_class(env, tc) do
    
      add_method = fn e -> fn sig -> 
        var_pairs = Nova.Array.map_with_index((fn i -> fn v -> {:tuple, v, (Nova.Compiler.Types.mk_tvar(((e.counter + i)), v))} end end), (Nova.Array.from_foldable(tc.type_vars)))
        var_map = Nova.Map.from_foldable(var_pairs)
        method_type = type_expr_to_type(var_map, sig.ty)
        scheme = Nova.Compiler.Types.mk_scheme((Nova.Runtime.map((&Nova.Runtime.snd/1), var_pairs)), method_type)
        Nova.Compiler.Types.extend_env(e, sig.name, scheme) end end
      Nova.Array.foldl(add_method, env, (Nova.Array.from_foldable(tc.methods)))
  end



  def check_data_type(env, dt) do
    check_data_type_with_all_aliases(Nova.Map.empty, Nova.Map.empty, env, dt)
  end



  def check_data_type_with_aliases(alias_map, env, dt) do
    check_data_type_with_all_aliases(alias_map, Nova.Map.empty, env, dt)
  end



  def check_data_type_with_all_aliases(alias_map, param_alias_map, env, dt) do
    
      type_var_pairs = Nova.Array.map_with_index((fn i -> fn v -> {:tuple, v, (Nova.Compiler.Types.mk_tvar(((env.counter + i)), v))} end end), (Nova.Array.from_foldable(dt.type_vars)))
      new_counter = (env.counter + Nova.List.length(dt.type_vars))
      type_var_map = Nova.Map.from_foldable(type_var_pairs)
      env1 = %{env | counter: new_counter}
      type_args = Nova.Runtime.map((fn ({:tuple, _, tv}) -> {:ty_var, tv} end), type_var_pairs)
      result_type = {:ty_con, (Nova.Compiler.Types.mk_tcon(dt.name, type_args))}
      add_constructor = fn e -> fn con -> 
        con_type = build_constructor_type_with_all_aliases(alias_map, param_alias_map, type_var_map, con.fields, result_type)
        con_scheme = Nova.Compiler.Types.mk_scheme((Nova.Runtime.map((&Nova.Runtime.snd/1), type_var_pairs)), con_type)
        Nova.Compiler.Types.extend_env(e, con.name, con_scheme) end end
      Nova.Array.foldl(add_constructor, env1, (Nova.Array.from_foldable(dt.constructors)))
  end



  def check_newtype_with_aliases(alias_map, env, nt) do
    check_newtype_with_all_aliases(alias_map, Nova.Map.empty, env, nt)
  end



  def check_newtype_with_all_aliases(alias_map, param_alias_map, env, nt) do
    
      type_var_pairs = Nova.Array.map_with_index((fn i -> fn v -> {:tuple, v, (Nova.Compiler.Types.mk_tvar(((env.counter + i)), v))} end end), (Nova.Array.from_foldable(nt.type_vars)))
      new_counter = (env.counter + Nova.List.length(nt.type_vars))
      type_var_map = Nova.Map.from_foldable(type_var_pairs)
      env1 = %{env | counter: new_counter}
      type_args = Nova.Runtime.map((fn ({:tuple, _, tv}) -> {:ty_var, tv} end), type_var_pairs)
      result_type = {:ty_con, (Nova.Compiler.Types.mk_tcon(nt.name, type_args))}
      wrapped_ty = type_expr_to_type_with_all_aliases(alias_map, param_alias_map, type_var_map, nt.wrapped_type)
      con_type = Nova.Compiler.Types.t_arrow(wrapped_ty, result_type)
      con_scheme = Nova.Compiler.Types.mk_scheme((Nova.Runtime.map((&Nova.Runtime.snd/1), type_var_pairs)), con_type)
      Nova.Compiler.Types.extend_env(env1, nt.constructor, con_scheme)
  end



  def build_constructor_type_go(_, result_type, []) do
    result_type
  end

  def build_constructor_type_go(var_map, result_type, ([field | rest])) do
    
      field_ty = type_expr_to_type(var_map, field.ty)
      Nova.Compiler.Types.t_arrow(field_ty, (build_constructor_type_go(var_map, result_type, rest)))
  end



  def build_constructor_type(var_map, fields, result_type) do
    build_constructor_type_go(var_map, result_type, fields)
  end



  def build_constructor_type_with_aliases_go(_, _, result_type, []) do
    result_type
  end

  def build_constructor_type_with_aliases_go(alias_map, var_map, result_type, ([field | rest])) do
    
      field_ty = type_expr_to_type_with_aliases(alias_map, var_map, field.ty)
      Nova.Compiler.Types.t_arrow(field_ty, (build_constructor_type_with_aliases_go(alias_map, var_map, result_type, rest)))
  end



  def build_constructor_type_with_aliases(alias_map, var_map, fields, result_type) do
    build_constructor_type_with_aliases_go(alias_map, var_map, result_type, fields)
  end



  def build_constructor_type_with_all_aliases_go(_, _, _, result_type, []) do
    result_type
  end

  def build_constructor_type_with_all_aliases_go(alias_map, param_alias_map, var_map, result_type, ([field | rest])) do
    
      field_ty = type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, field.ty)
      Nova.Compiler.Types.t_arrow(field_ty, (build_constructor_type_with_all_aliases_go(alias_map, param_alias_map, var_map, result_type, rest)))
  end



  def build_constructor_type_with_all_aliases(alias_map, param_alias_map, var_map, fields, result_type) do
    build_constructor_type_with_all_aliases_go(alias_map, param_alias_map, var_map, result_type, fields)
  end



  def type_expr_to_type_with_aliases(alias_map, var_map, expr) do
    type_expr_to_type_with_all_aliases(alias_map, Nova.Map.empty, var_map, expr)
  end



  def type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, ({:ty_expr_var, name})) do
    case Nova.Map.lookup(name, var_map) do
      {:just, tv} -> {:ty_var, tv}
      :nothing -> {:ty_con, (Nova.Compiler.Types.mk_tcon(name, []))}
    end
  end

  def type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, ({:ty_expr_con, name})) do
    
      unqualified_name = case Nova.String.last_index_of((Nova.String.pattern(".")), name) do
        {:just, idx} -> Nova.String.drop(((idx + 1)), name)
        :nothing -> name
      end
      try_lookup = fn nm -> case Nova.Map.lookup(nm, alias_map) do
        {:just, ty} -> {:just, ty}
        :nothing -> case Nova.Map.lookup(nm, param_alias_map) do
            {:just, info} ->
              cond do
                Nova.Array.null(info.params) -> {:just, (type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, info.body))}
                true -> :nothing
              end
            _ -> :nothing
          end
      end end
      case try_lookup.(name) do
  {:just, ty} -> ty
  :nothing -> case try_lookup.(unqualified_name) do
      {:just, ty} -> ty
      :nothing -> type_expr_to_type(var_map, (Nova.Compiler.Ast.ty_expr_con(name)))
    end
end
  end

  def type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, ({:ty_expr_app, f, arg})) do
    case collect_type_app((Nova.Compiler.Ast.ty_expr_app(f, arg))) do
      {:tuple, con_name, args} -> case Nova.Map.lookup(con_name, param_alias_map) do
          {:just, info} ->
            cond do
              (Nova.Array.length(info.params) == Nova.Array.length(args)) -> 
                  arg_types = Nova.Runtime.map((fn auto_p0 -> type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, auto_p0) end), args)
                  param_subst = Nova.Map.from_foldable((Nova.Array.zip(info.params, arg_types)))
                  substitute_type_expr(alias_map, param_alias_map, param_subst, info.body)
              true -> case type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, f) do
              {:ty_con, tc} -> {:ty_con, %{name: tc.name, args: Nova.Array.snoc(tc.args, (type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, arg)))}}
              other -> other
            end
            end
          _ -> case type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, f) do
              {:ty_con, tc} -> {:ty_con, %{name: tc.name, args: Nova.Array.snoc(tc.args, (type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, arg)))}}
              other -> other
            end
        end
    end
  end

  def type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, ({:ty_expr_arrow, a, b})) do
    Nova.Compiler.Types.t_arrow((type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, a)), (type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, b)))
  end

  def type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, ({:ty_expr_record, fields, maybe_row})) do
    
      field_map = Nova.Map.from_foldable((Nova.Runtime.map((fn ({:tuple, l, t}) -> {:tuple, l, (type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, t))} end), fields)))
      row = case maybe_row do
        {:just, r} -> case Nova.Map.lookup(r, var_map) do
            {:just, tv} -> {:just, tv}
            :nothing -> :nothing
          end
        :nothing -> :nothing
      end
      {:ty_record, %{fields: field_map, row: row}}
  end

  def type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, ({:ty_expr_for_all, vars, t})) do
    
      var_list = Nova.Array.map_with_index((fn i -> fn name -> {:tuple, name, (Nova.Compiler.Types.mk_tvar((-((i + 1))), name))} end end), (Nova.Array.from_foldable(vars)))
      new_var_map = Nova.Map.union((Nova.Map.from_foldable(var_list)), var_map)
      type_expr_to_type_with_all_aliases(alias_map, param_alias_map, new_var_map, t)
  end

  def type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, ({:ty_expr_constrained, _, t})) do
    type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, t)
  end

  def type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, ({:ty_expr_parens, t})) do
    type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, t)
  end

  def type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, ({:ty_expr_tuple, ts})) do
    Nova.Compiler.Types.t_tuple((Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> type_expr_to_type_with_all_aliases(alias_map, param_alias_map, var_map, auto_p0) end), ts)))))
  end



  def type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, ({:ty_expr_var, name})) do
    case Nova.Map.lookup(name, var_map) do
      {:just, tv} -> {:ty_var, tv}
      :nothing -> {:ty_con, (Nova.Compiler.Types.mk_tcon(name, []))}
    end
  end

  def type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, ({:ty_expr_con, name})) do
    
      unqualified_name = case Nova.String.last_index_of((Nova.String.pattern(".")), name) do
        {:just, idx} -> Nova.String.drop(((idx + 1)), name)
        :nothing -> name
      end
      try_lookup = fn nm -> case Nova.Map.lookup(nm, alias_map) do
        {:just, ty} -> {:just, ty}
        :nothing -> case Nova.Map.lookup(nm, param_alias_map) do
            {:just, info} ->
              cond do
                Nova.Array.null(info.params) -> {:just, (type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, info.body))}
                true -> :nothing
              end
            _ -> :nothing
          end
      end end
      try_env_lookup = fn nm -> case Nova.Compiler.Types.lookup_env(env, nm) do
        {:just, scheme} -> {:just, scheme.ty}
        :nothing -> :nothing
      end end
      case try_lookup.(name) do
  {:just, ty} -> ty
  :nothing -> case try_lookup.(unqualified_name) do
      {:just, ty} -> ty
      :nothing -> case try_env_lookup.(name) do
          {:just, ty} -> ty
          :nothing -> case try_env_lookup.(unqualified_name) do
              {:just, ty} -> ty
              :nothing -> type_expr_to_type(var_map, (Nova.Compiler.Ast.ty_expr_con(name)))
            end
        end
    end
end
  end

  def type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, ({:ty_expr_app, f, arg})) do
    case collect_type_app((Nova.Compiler.Ast.ty_expr_app(f, arg))) do
      {:tuple, con_name, args} -> case Nova.Map.lookup(con_name, param_alias_map) do
          {:just, info} ->
            cond do
              (Nova.Array.length(info.params) == Nova.Array.length(args)) -> 
                  arg_types = Nova.Runtime.map((fn auto_p0 -> type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, auto_p0) end), args)
                  param_subst = Nova.Map.from_foldable((Nova.Array.zip(info.params, arg_types)))
                  substitute_type_expr(alias_map, param_alias_map, param_subst, info.body)
              true -> case type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, f) do
              {:ty_con, tc} -> {:ty_con, %{name: tc.name, args: Nova.Array.snoc(tc.args, (type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, arg)))}}
              other -> other
            end
            end
          _ -> case type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, f) do
              {:ty_con, tc} -> {:ty_con, %{name: tc.name, args: Nova.Array.snoc(tc.args, (type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, arg)))}}
              other -> other
            end
        end
    end
  end

  def type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, ({:ty_expr_arrow, a, b})) do
    Nova.Compiler.Types.t_arrow((type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, a)), (type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, b)))
  end

  def type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, ({:ty_expr_record, fields, maybe_row})) do
    
      field_map = Nova.Map.from_foldable((Nova.Runtime.map((fn ({:tuple, l, t}) -> {:tuple, l, (type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, t))} end), fields)))
      row = case maybe_row do
        {:just, r} -> case Nova.Map.lookup(r, var_map) do
            {:just, tv} -> {:just, tv}
            :nothing -> :nothing
          end
        :nothing -> :nothing
      end
      {:ty_record, %{fields: field_map, row: row}}
  end

  def type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, ({:ty_expr_for_all, vars, t})) do
    
      var_list = Nova.Array.map_with_index((fn i -> fn name -> {:tuple, name, (Nova.Compiler.Types.mk_tvar((-((i + 1))), name))} end end), (Nova.Array.from_foldable(vars)))
      new_var_map = Nova.Map.union((Nova.Map.from_foldable(var_list)), var_map)
      type_expr_to_type_with_env(env, alias_map, param_alias_map, new_var_map, t)
  end

  def type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, ({:ty_expr_constrained, _, t})) do
    type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, t)
  end

  def type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, ({:ty_expr_parens, t})) do
    type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, t)
  end

  def type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, ({:ty_expr_tuple, ts})) do
    Nova.Compiler.Types.t_tuple((Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> type_expr_to_type_with_env(env, alias_map, param_alias_map, var_map, auto_p0) end), ts)))))
  end



  def collect_type_app(({:ty_expr_con, name})) do
    {:tuple, name, []}
  end

  def collect_type_app(({:ty_expr_app, f, arg})) do
    
      {:tuple, name, args} = collect_type_app(f)
      {:tuple, name, (Nova.Array.snoc(args, arg))}
  end

  def collect_type_app(_) do
    {:tuple, "", []}
  end



  def substitute_type_expr(alias_map, param_alias_map, param_subst, ({:ty_expr_var, name})) do
    case Nova.Map.lookup(name, param_subst) do
      {:just, ty} -> ty
      :nothing -> {:ty_con, (Nova.Compiler.Types.mk_tcon(name, []))}
    end
  end

  def substitute_type_expr(alias_map, param_alias_map, param_subst, ({:ty_expr_con, name})) do
    case Nova.Map.lookup(name, alias_map) do
      {:just, ty} -> ty
      :nothing -> type_expr_to_type(Nova.Map.empty, (Nova.Compiler.Ast.ty_expr_con(name)))
    end
  end

  def substitute_type_expr(alias_map, param_alias_map, param_subst, ({:ty_expr_app, f, arg})) do
    case collect_type_app((Nova.Compiler.Ast.ty_expr_app(f, arg))) do
      {:tuple, con_name, args} -> case Nova.Map.lookup(con_name, param_alias_map) do
          {:just, info} ->
            cond do
              (Nova.Array.length(info.params) == Nova.Array.length(args)) -> 
                  arg_types = Nova.Runtime.map((fn auto_p0 -> substitute_type_expr(alias_map, param_alias_map, param_subst, auto_p0) end), args)
                  nested_subst = Nova.Map.from_foldable((Nova.Array.zip(info.params, arg_types)))
                  substitute_type_expr(alias_map, param_alias_map, nested_subst, info.body)
              true -> case substitute_type_expr(alias_map, param_alias_map, param_subst, f) do
              {:ty_con, tc} -> {:ty_con, %{name: tc.name, args: Nova.Array.snoc(tc.args, (substitute_type_expr(alias_map, param_alias_map, param_subst, arg)))}}
              other -> other
            end
            end
          _ -> case substitute_type_expr(alias_map, param_alias_map, param_subst, f) do
              {:ty_con, tc} -> {:ty_con, %{name: tc.name, args: Nova.Array.snoc(tc.args, (substitute_type_expr(alias_map, param_alias_map, param_subst, arg)))}}
              other -> other
            end
        end
    end
  end

  def substitute_type_expr(alias_map, param_alias_map, param_subst, ({:ty_expr_arrow, a, b})) do
    Nova.Compiler.Types.t_arrow((substitute_type_expr(alias_map, param_alias_map, param_subst, a)), (substitute_type_expr(alias_map, param_alias_map, param_subst, b)))
  end

  def substitute_type_expr(alias_map, param_alias_map, param_subst, ({:ty_expr_record, fields, maybe_row})) do
    
      field_map = Nova.Map.from_foldable((Nova.Runtime.map((fn ({:tuple, l, t}) -> {:tuple, l, (substitute_type_expr(alias_map, param_alias_map, param_subst, t))} end), fields)))
      {:ty_record, %{fields: field_map, row: :nothing}}
  end

  def substitute_type_expr(alias_map, param_alias_map, param_subst, ({:ty_expr_tuple, ts})) do
    Nova.Compiler.Types.t_tuple((Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> substitute_type_expr(alias_map, param_alias_map, param_subst, auto_p0) end), ts)))))
  end

  def substitute_type_expr(alias_map, param_alias_map, param_subst, ({:ty_expr_parens, t})) do
    substitute_type_expr(alias_map, param_alias_map, param_subst, t)
  end

  def substitute_type_expr(alias_map, param_alias_map, param_subst, ({:ty_expr_for_all, _, t})) do
    substitute_type_expr(alias_map, param_alias_map, param_subst, t)
  end

  def substitute_type_expr(alias_map, param_alias_map, param_subst, ({:ty_expr_constrained, _, t})) do
    substitute_type_expr(alias_map, param_alias_map, param_subst, t)
  end



  def type_expr_to_type(var_map, ({:ty_expr_var, name})) do
    case Nova.Map.lookup(name, var_map) do
      {:just, tv} -> {:ty_var, tv}
      :nothing -> {:ty_con, (Nova.Compiler.Types.mk_tcon(name, []))}
    end
  end

  def type_expr_to_type(var_map, ({:ty_expr_con, name})) do
    
      t_type_holder = {:ty_con, (Nova.Compiler.Types.mk_tcon("Type", []))}
      t_tvar_holder = {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "id", Nova.Compiler.Types.t_int()}, {:tuple, "name", Nova.Compiler.Types.t_string()}]), row: :nothing}}
      t_pattern_holder = {:ty_con, (Nova.Compiler.Types.mk_tcon("Pattern", []))}
      t_expr_holder = {:ty_con, (Nova.Compiler.Types.mk_tcon("Expr", []))}
      t_guard_clause_holder = {:ty_con, (Nova.Compiler.Types.mk_tcon("GuardClause", []))}
      t_type_expr_holder = {:ty_con, (Nova.Compiler.Types.mk_tcon("TypeExpr", []))}
      t_scheme_holder = {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "vars", (Nova.Compiler.Types.t_array(t_tvar_holder))}, {:tuple, "ty", t_type_holder}]), row: :nothing}}
      t_let_bind_holder = {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "pattern", t_pattern_holder}, {:tuple, "value", t_expr_holder}, {:tuple, "typeAnn", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Maybe", [t_type_expr_holder]))})}]), row: :nothing}}
      t_guarded_expr_holder = {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "guards", (Nova.Compiler.Types.t_array(t_guard_clause_holder))}, {:tuple, "body", t_expr_holder}]), row: :nothing}}
      t_data_field_holder = {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "label", Nova.Compiler.Types.t_string()}, {:tuple, "ty", t_type_expr_holder}]), row: :nothing}}
      t_constraint_holder = {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "className", Nova.Compiler.Types.t_string()}, {:tuple, "types", (Nova.Compiler.Types.t_array(t_type_expr_holder))}]), row: :nothing}}
      t_type_sig_holder = {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "typeVars", (Nova.Compiler.Types.t_array(Nova.Compiler.Types.t_string()))}, {:tuple, "constraints", (Nova.Compiler.Types.t_array(t_constraint_holder))}, {:tuple, "ty", t_type_expr_holder}]), row: :nothing}}
      t_data_constructor_holder = {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "fields", (Nova.Compiler.Types.t_array(t_data_field_holder))}, {:tuple, "isRecord", Nova.Compiler.Types.t_bool()}]), row: :nothing}}
      case name do
  "_" -> {:ty_var, (Nova.Compiler.Types.mk_tvar((-999), "_"))}
  "Boolean" -> Nova.Compiler.Types.t_bool()
  "TCon" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "args", (Nova.Compiler.Types.t_array(t_type_holder))}]), row: :nothing}}
  "TVar" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "id", Nova.Compiler.Types.t_int()}, {:tuple, "name", Nova.Compiler.Types.t_string()}]), row: :nothing}}
  "Token" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "tokenType", ({:ty_con, (Nova.Compiler.Types.mk_tcon("TokenType", []))})}, {:tuple, "value", Nova.Compiler.Types.t_string()}, {:tuple, "line", Nova.Compiler.Types.t_int()}, {:tuple, "column", Nova.Compiler.Types.t_int()}, {:tuple, "pos", Nova.Compiler.Types.t_int()}]), row: :nothing}}
  "Subst" -> {:ty_con, (Nova.Compiler.Types.mk_tcon("Map", [Nova.Compiler.Types.t_int(), t_type_holder]))}
  "Env" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "bindings", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Map", [Nova.Compiler.Types.t_string(), t_scheme_holder]))})}, {:tuple, "counter", Nova.Compiler.Types.t_int()}, {:tuple, "registryLayer", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Maybe", [Nova.Compiler.Types.t_int()]))})}, {:tuple, "namespace", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Maybe", [Nova.Compiler.Types.t_string()]))})}]), row: :nothing}}
  "Scheme" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "vars", (Nova.Compiler.Types.t_array(t_tvar_holder))}, {:tuple, "ty", t_type_holder}]), row: :nothing}}
  "FunctionDeclaration" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "parameters", (Nova.Compiler.Types.t_array(t_pattern_holder))}, {:tuple, "body", t_expr_holder}, {:tuple, "guards", (Nova.Compiler.Types.t_array(t_guarded_expr_holder))}, {:tuple, "typeSignature", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Maybe", [t_type_sig_holder]))})}, {:tuple, "whereBindings", (Nova.Compiler.Types.t_array(t_let_bind_holder))}]), row: :nothing}}
  "DataConstructor" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "fields", (Nova.Compiler.Types.t_array(t_data_field_holder))}]), row: :nothing}}
  "DataField" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Maybe", [Nova.Compiler.Types.t_string()]))})}, {:tuple, "ty", t_type_expr_holder}]), row: :nothing}}
  "TypeSignature" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "typeVars", (Nova.Compiler.Types.t_array(Nova.Compiler.Types.t_string()))}, {:tuple, "constraints", (Nova.Compiler.Types.t_array(t_constraint_holder))}, {:tuple, "ty", t_type_expr_holder}]), row: :nothing}}
  "LetBind" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "pattern", t_pattern_holder}, {:tuple, "value", t_expr_holder}, {:tuple, "typeAnn", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Maybe", [t_type_expr_holder]))})}]), row: :nothing}}
  "Ast.LetBind" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "pattern", t_pattern_holder}, {:tuple, "value", t_expr_holder}, {:tuple, "typeAnn", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Maybe", [t_type_expr_holder]))})}]), row: :nothing}}
  "CaseClause" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "pattern", t_pattern_holder}, {:tuple, "guard", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Maybe", [t_expr_holder]))})}, {:tuple, "body", t_expr_holder}]), row: :nothing}}
  "Ast.CaseClause" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "pattern", t_pattern_holder}, {:tuple, "guard", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Maybe", [t_expr_holder]))})}, {:tuple, "body", t_expr_holder}]), row: :nothing}}
  "GuardedExpr" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "guards", (Nova.Compiler.Types.t_array(t_guard_clause_holder))}, {:tuple, "body", t_expr_holder}]), row: :nothing}}
  "Ast.GuardedExpr" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "guards", (Nova.Compiler.Types.t_array(t_guard_clause_holder))}, {:tuple, "body", t_expr_holder}]), row: :nothing}}
  "GuardClause" -> {:ty_con, (Nova.Compiler.Types.mk_tcon("GuardClause", []))}
  "Ast.GuardClause" -> {:ty_con, (Nova.Compiler.Types.mk_tcon("GuardClause", []))}
  "Constraint" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "className", Nova.Compiler.Types.t_string()}, {:tuple, "types", (Nova.Compiler.Types.t_array(t_type_expr_holder))}]), row: :nothing}}
  "Ast.Constraint" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "className", Nova.Compiler.Types.t_string()}, {:tuple, "types", (Nova.Compiler.Types.t_array(t_type_expr_holder))}]), row: :nothing}}
  "TypeAlias" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "typeVars", (Nova.Compiler.Types.t_array(Nova.Compiler.Types.t_string()))}, {:tuple, "ty", t_type_expr_holder}]), row: :nothing}}
  "Ast.TypeAlias" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "typeVars", (Nova.Compiler.Types.t_array(Nova.Compiler.Types.t_string()))}, {:tuple, "ty", t_type_expr_holder}]), row: :nothing}}
  "DataType" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "typeVars", (Nova.Compiler.Types.t_array(Nova.Compiler.Types.t_string()))}, {:tuple, "constructors", (Nova.Compiler.Types.t_array(t_data_constructor_holder))}]), row: :nothing}}
  "Ast.DataType" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "typeVars", (Nova.Compiler.Types.t_array(Nova.Compiler.Types.t_string()))}, {:tuple, "constructors", (Nova.Compiler.Types.t_array(t_data_constructor_holder))}]), row: :nothing}}
  "DataConstructor" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "fields", (Nova.Compiler.Types.t_array(t_data_field_holder))}, {:tuple, "isRecord", Nova.Compiler.Types.t_bool()}]), row: :nothing}}
  "Ast.DataConstructor" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "fields", (Nova.Compiler.Types.t_array(t_data_field_holder))}, {:tuple, "isRecord", Nova.Compiler.Types.t_bool()}]), row: :nothing}}
  "ImportDeclaration" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "moduleName", Nova.Compiler.Types.t_string()}, {:tuple, "alias", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Maybe", [Nova.Compiler.Types.t_string()]))})}, {:tuple, "items", (Nova.Compiler.Types.t_array(({:ty_con, (Nova.Compiler.Types.mk_tcon("ImportItem", []))})))}, {:tuple, "hiding", Nova.Compiler.Types.t_bool()}]), row: :nothing}}
  "ModuleDeclaration" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}]), row: :nothing}}
  "GenCtx" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "moduleFuncs", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Set", [Nova.Compiler.Types.t_string()]))})}, {:tuple, "locals", ({:ty_con, (Nova.Compiler.Types.mk_tcon("Set", [Nova.Compiler.Types.t_string()]))})}]), row: :nothing}}
  "Module" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", Nova.Compiler.Types.t_string()}, {:tuple, "declarations", (Nova.Compiler.Types.t_array(({:ty_con, (Nova.Compiler.Types.mk_tcon("Declaration", []))})))}]), row: :nothing}}
  "TypeAliasInfo" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "params", (Nova.Compiler.Types.t_array(Nova.Compiler.Types.t_string()))}, {:tuple, "body", t_type_expr_holder}]), row: :nothing}}
  "TypeInfo" -> {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "arity", Nova.Compiler.Types.t_int()}, {:tuple, "constructors", (Nova.Compiler.Types.t_array(Nova.Compiler.Types.t_string()))}]), row: :nothing}}
  _ -> {:ty_con, (Nova.Compiler.Types.mk_tcon(name, []))}
end
  end

  def type_expr_to_type(var_map, ({:ty_expr_app, f, arg})) do
    case type_expr_to_type(var_map, f) do
      {:ty_con, tc} -> {:ty_con, %{name: tc.name, args: Nova.Array.snoc(tc.args, (type_expr_to_type(var_map, arg)))}}
      other -> other
    end
  end

  def type_expr_to_type(var_map, ({:ty_expr_arrow, a, b})) do
    Nova.Compiler.Types.t_arrow((type_expr_to_type(var_map, a)), (type_expr_to_type(var_map, b)))
  end

  def type_expr_to_type(var_map, ({:ty_expr_record, fields, maybe_row})) do
    
      field_map = Nova.Map.from_foldable((Nova.Runtime.map((fn ({:tuple, l, t}) -> {:tuple, l, (type_expr_to_type(var_map, t))} end), fields)))
      row = case maybe_row do
        {:just, r} -> case Nova.Map.lookup(r, var_map) do
            {:just, tv} -> {:just, tv}
            :nothing -> :nothing
          end
        :nothing -> :nothing
      end
      {:ty_record, %{fields: field_map, row: row}}
  end

  def type_expr_to_type(var_map, ({:ty_expr_for_all, vars, t})) do
    
      var_list = list_map_with_index((fn i -> fn name -> {:tuple, name, (Nova.Compiler.Types.mk_tvar((-((i + 1))), name))} end end), vars)
      new_var_map = Nova.Map.union((Nova.Map.from_foldable(var_list)), var_map)
      type_expr_to_type(new_var_map, t)
  end

  def type_expr_to_type(var_map, ({:ty_expr_constrained, _, t})) do
    type_expr_to_type(var_map, t)
  end

  def type_expr_to_type(var_map, ({:ty_expr_parens, t})) do
    type_expr_to_type(var_map, t)
  end

  def type_expr_to_type(var_map, ({:ty_expr_tuple, ts})) do
    Nova.Compiler.Types.t_tuple((Nova.Array.from_foldable((Nova.Runtime.map((fn auto_p0 -> type_expr_to_type(var_map, auto_p0) end), ts)))))
  end



  def check_type_alias(env, ta) do
    
      ty = type_expr_to_type(Nova.Map.empty, ta.ty)
      scheme = Nova.Compiler.Types.mk_scheme([], ty)
      Nova.Compiler.Types.extend_env(env, ta.name, scheme)
  end



  def check_module(env, decls) do
    
      env1 = process_non_functions(env, decls)
      env2 = add_function_placeholders(env1, decls)
      check_function_bodies(env2, decls)
  end



  def check_module_with_registry(registry, env, decls) do
    
      imported_aliases = collect_imported_aliases(registry, decls)
      env1 = process_imports(registry, env, decls)
      env2 = process_non_functions_with_aliases(imported_aliases, env1, decls)
      env3 = add_function_placeholders_with_aliases(imported_aliases, env2, decls)
      check_function_bodies(env3, decls)
  end



  def process_imports(registry, env, decls) do
    
      process_import = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {e, ({:decl_import, imp})} -> process_import_decl(registry, e, imp)
        {e, _} -> e
      end end end
      Nova.Array.foldl(process_import, env, decls)
  end



  def process_import_decl(registry, env, imp) do
    case Nova.Compiler.Types.lookup_module(registry, imp.module_name) do
      :nothing -> env
      {:just, exports} -> 
          env_with_qualified = case imp.alias_ do
            {:just, alias_} -> Nova.Compiler.Types.merge_exports_to_env_with_prefix(env, exports, alias_)
            :nothing -> 
                last_part = case Nova.String.last_index_of((Nova.String.pattern(".")), imp.module_name) do
                  :nothing -> imp.module_name
                  {:just, idx} -> Nova.String.drop(((idx + 1)), imp.module_name)
                end
                Nova.Compiler.Types.merge_exports_to_env_with_prefix(env, exports, last_part)
          end
          if imp.hiding do
  Nova.Compiler.Types.merge_exports_to_env(env_with_qualified, exports)
else
  if Nova.List.null(imp.items) do
    Nova.Compiler.Types.merge_exports_to_env(env_with_qualified, exports)
  else
    Nova.Runtime.foldl((fn auto_p0 -> fn auto_p1 -> import_item(exports, auto_p0, auto_p1) end end), env_with_qualified, imp.items)
  end
end
    end
  end



  def import_item(exports, env, item) do
    case item do
      {:import_value, name} -> case Nova.Map.lookup(name, exports.values) do
          {:just, scheme} -> Nova.Compiler.Types.extend_env(env, name, scheme)
          :nothing -> case Nova.Map.lookup(name, exports.constructors) do
              {:just, scheme} -> Nova.Compiler.Types.extend_env(env, name, scheme)
              :nothing -> env
            end
        end
      {:import_type, type_name, spec} -> case Nova.Map.lookup(type_name, exports.types) do
          :nothing -> env
          {:just, type_info} -> case spec do
              :import_all -> Nova.Compiler.Types.merge_type_export(env, exports, type_name, type_info.constructors)
              {:import_some, ctor_names} -> Nova.Compiler.Types.merge_type_export(env, exports, type_name, (Nova.Array.from_foldable(ctor_names)))
              :import_none -> env
            end
        end
    end
  end



  def collect_imported_aliases(registry, decls) do
    
      collect_from_import = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {acc, ({:decl_import, imp})} -> case Nova.Compiler.Types.lookup_module(registry, imp.module_name) do
  :nothing -> acc
  {:just, exports} -> Nova.Map.union(acc, exports.type_aliases)
end
        {acc, _} -> acc
      end end end
      Nova.Array.foldl(collect_from_import, Nova.Map.empty, decls)
  end



  def extract_exports(decls) do
    
      alias_map = collect_type_aliases(decls)
      param_alias_map = collect_param_type_aliases(decls)
      add_constructor_placeholder = fn dt -> fn exp -> fn ctor -> 
        type_var_pairs = Nova.Array.map_with_index((fn i -> fn v -> {:tuple, v, (Nova.Compiler.Types.mk_tvar(i, v))} end end), (Nova.Array.from_foldable(dt.type_vars)))
        type_var_map = Nova.Map.from_foldable(type_var_pairs)
        result_type = if Nova.List.null(dt.type_vars) do
          {:ty_con, (Nova.Compiler.Types.mk_tcon(dt.name, []))}
        else
          {:ty_con, (Nova.Compiler.Types.mk_tcon(dt.name, (Nova.Runtime.map((fn ({:tuple, _, tv}) -> {:ty_var, tv} end), type_var_pairs))))}
        end
        ctor_type = build_constructor_type_with_aliases(alias_map, type_var_map, ctor.fields, result_type)
        scheme = Nova.Compiler.Types.mk_scheme((Nova.Runtime.map((&Nova.Runtime.snd/1), type_var_pairs)), ctor_type)
        %{exp | constructors: Nova.Map.insert(ctor.name, scheme, exp.constructors)} end end end
      collect_export = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {exp, ({:decl_data_type, dt})} -> 
  type_info = %{arity: Nova.List.length(dt.type_vars), constructors: Nova.Array.from_foldable((Nova.Runtime.map(& &1.name, dt.constructors)))}
  exp1 = %{exp | types: Nova.Map.insert(dt.name, type_info, exp.types)}
  Nova.Array.foldl((add_constructor_placeholder.(dt)), exp1, (Nova.Array.from_foldable(dt.constructors)))
        {exp, ({:decl_type_alias, ta})} -> %{exp | type_aliases: Nova.Map.insert(ta.name, %{params: Nova.Array.from_foldable(ta.type_vars), body: ta.ty}, exp.type_aliases)}
        {exp, ({:decl_function, func})} -> exp
        {exp, ({:decl_foreign_import, fi})} -> 
  ty = type_expr_to_type(Nova.Map.empty, fi.type_signature)
  free_vars = Nova.Array.from_foldable((Nova.Set.to_unfoldable((Nova.Compiler.Types.free_type_vars(ty)))))
  tvars = Nova.Runtime.map((fn id -> %{id: id, name: Nova.Runtime.append("a", Nova.Runtime.show(id))} end), free_vars)
  scheme = Nova.Compiler.Types.mk_scheme(tvars, ty)
  %{exp | values: Nova.Map.insert(fi.function_name, scheme, exp.values)}
        {exp, _} -> exp
      end end end
      Nova.Array.foldl(collect_export, Nova.Compiler.Types.empty_exports(), decls)
  end



  def add_values_to_exports(exports, env, decls) do
    
      add_value = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {exp, ({:decl_function, func})} -> case Nova.Map.lookup(func.name, env.bindings) do
  {:just, scheme} -> %{exp | values: Nova.Map.insert(func.name, scheme, exp.values)}
  :nothing -> exp
end
        {exp, _} -> exp
      end end end
      Nova.Array.foldl(add_value, exports, decls)
  end



  def collect_type_aliases(decls) do
    
      collect = fn m -> fn decl -> case decl do
        {:decl_type_alias, ta} -> if Nova.List.null(ta.type_vars) do
            Nova.Map.insert(ta.name, (type_expr_to_type(Nova.Map.empty, ta.ty)), m)
          else
            m
          end
        _ -> m
      end end end
      Nova.Array.foldl(collect, Nova.Map.empty, decls)
  end



  def collect_param_type_aliases(decls) do
    
      collect = fn auto_arg0 -> fn auto_arg1 -> case {auto_arg0, auto_arg1} do
        {m, ({:decl_type_alias, ta})} -> Nova.Map.insert(ta.name, %{params: Nova.Array.from_foldable(ta.type_vars), body: ta.ty}, m)
        {m, _} -> m
      end end end
      Nova.Array.foldl(collect, Nova.Map.empty, decls)
  end



  def process_non_functions(env, decls) do
    process_non_functions_with_aliases(Nova.Map.empty, env, decls)
  end



  def process_non_functions_with_aliases(imported_aliases, env, decls) do
    
      local_alias_map = collect_type_aliases(decls)
      local_param_alias_map = collect_param_type_aliases(decls)
      imported_simple_aliases = Nova.Map.map_maybe((fn info -> if Nova.Array.null(info.params) do
        {:just, (type_expr_to_type(Nova.Map.empty, info.body))}
      else
        :nothing
      end end), imported_aliases)
      process_type_alias = fn e -> fn decl -> case decl do
        {:decl_type_alias, ta} -> check_type_alias(e, ta)
        _ -> e
      end end end
      process_type_class = fn e -> fn decl -> case decl do
        {:decl_type_class, tc} -> check_type_class(e, tc)
        _ -> e
      end end end
      param_alias_map = Nova.Map.union(local_param_alias_map, imported_aliases)
      alias_map = Nova.Map.union(local_alias_map, imported_simple_aliases)
      env1 = Nova.Array.foldl(process_type_alias, env, decls)
      process_data_type = fn e -> fn decl -> case decl do
        {:decl_data_type, dt} -> check_data_type_with_all_aliases(alias_map, param_alias_map, e, dt)
        {:decl_newtype, nt} -> check_newtype_with_all_aliases(alias_map, param_alias_map, e, nt)
        _ -> e
      end end end
      env2 = Nova.Array.foldl(process_data_type, env1, decls)
      env3 = Nova.Array.foldl(process_type_class, env2, decls)
      env3
  end



  def add_function_placeholders(env, decls) do
    add_function_placeholders_with_aliases(Nova.Map.empty, env, decls)
  end



  def add_function_placeholders_with_aliases(imported_aliases, env, decls) do
    
      local_alias_map = collect_type_aliases(decls)
      local_param_alias_map = collect_param_type_aliases(decls)
      imported_simple_aliases = Nova.Map.map_maybe((fn info -> if Nova.Array.null(info.params) do
        {:just, (type_expr_to_type(Nova.Map.empty, info.body))}
      else
        :nothing
      end end), imported_aliases)
      collect_sig = fn m -> fn decl -> case decl do
        {:decl_type_sig, sig} -> Nova.Map.insert(sig.name, sig.ty, m)
        _ -> m
      end end end
      param_alias_map = Nova.Map.union(local_param_alias_map, imported_aliases)
      alias_map = Nova.Map.union(local_alias_map, imported_simple_aliases)
      sig_map = Nova.Array.foldl(collect_sig, Nova.Map.empty, decls)
      add_placeholder = fn e -> fn decl -> case decl do
        {:decl_function, func} -> case func.type_signature do
            {:just, sig} -> 
                ty = type_expr_to_type_with_all_aliases(alias_map, param_alias_map, Nova.Map.empty, sig.ty)
                scheme = Nova.Compiler.Types.mk_scheme([], ty)
                Nova.Compiler.Types.extend_env(e, func.name, scheme)
            :nothing -> case Nova.Map.lookup(func.name, sig_map) do
                {:just, ty_expr} -> 
                    ty = type_expr_to_type_with_all_aliases(alias_map, param_alias_map, Nova.Map.empty, ty_expr)
                    scheme = Nova.Compiler.Types.mk_scheme([], ty)
                    Nova.Compiler.Types.extend_env(e, func.name, scheme)
                :nothing -> 
                    {:tuple, tv, e_prime} = Nova.Compiler.Types.fresh_var(e, (Nova.Runtime.append("fn_", func.name)))
                    Nova.Compiler.Types.extend_env(e_prime, func.name, (Nova.Compiler.Types.mk_scheme([], ({:ty_var, tv}))))
              end
          end
        _ -> e
      end end end
      Nova.Array.foldl(add_placeholder, env, decls)
  end



  def check_function_bodies_go(e, ds) do
    case Nova.Array.uncons(ds) do
      :nothing -> {:right, e}
      {:just, %{head: {:decl_function, func}, tail: rest}} -> case check_function(e, func) do
          {:left, err} -> {:left, err}
          {:right, r} -> check_function_bodies_go(r.env, rest)
        end
      {:just, %{head: _, tail: rest}} -> check_function_bodies_go(e, rest)
    end
  end



  def check_function_bodies(env, decls) do
    
      merged_decls = merge_multi_clause_functions(decls)
      check_function_bodies_go(env, merged_decls)
  end



  def merge_multi_clause_functions(decls) do
    
      collect_same_name = Nova.Runtime.fix3(fn collect_same_name -> fn name -> fn ds -> fn acc -> case Nova.Array.uncons(ds) do
        :nothing -> %{same_name: acc, remaining: []}
        {:just, %{head: {:decl_function, f}, tail: rest}} -> if (f.name == name) do
            collect_same_name.(name).(rest).((Nova.Array.snoc(acc, f)))
          else
            %{same_name: acc, remaining: ds}
          end
        {:just, _} -> %{same_name: acc, remaining: ds}
      end  end end end end)
      clause_to_case_clause = fn _ -> fn func -> case Nova.List.head(func.parameters) do
        :nothing -> :nothing
        {:just, pat} -> {:just, %{pattern: pat, body: func.body, guard: :nothing}}
      end end end
      clause_to_tuple_case = fn _ -> fn func -> 
        n = Nova.List.length(func.parameters)
        tup_name = if (n == 2) do
          "Tuple"
        else
          Nova.Runtime.append("Tuple", Nova.Runtime.show(n))
        end
        if (n > 1) do
  {:just, %{pattern: Nova.Compiler.Ast.pat_con(tup_name, func.parameters), body: func.body, guard: :nothing}}
else
  clause_to_case_clause.([]).(func)
end end end
      merge_clauses_into_one = fn clauses -> case Nova.Array.head(clauses) do
        :nothing -> %{name: "", parameters: [], body: Nova.Compiler.Ast.expr_var("error"), guards: [], type_signature: :nothing}
        {:just, first} -> 
            name = first.name
            num_params = Nova.List.length(first.parameters)
            clauses_list = Nova.List.from_foldable(clauses)
            param_names = Nova.Runtime.map((fn i -> Nova.Runtime.append("__arg", Nova.Runtime.show(i)) end), (Nova.List.range(0, ((num_params - 1)))))
            param_pats = Nova.Runtime.map((&Nova.Compiler.Ast.pat_var/1), param_names)
            param_vars = Nova.Runtime.map((&Nova.Compiler.Ast.expr_var/1), param_names)
            case_clauses = Nova.List.map_maybe((clause_to_case_clause.(param_vars)), clauses_list)
            case_expr = case num_params do
              0 -> first.body
              1 -> case Nova.List.head(param_vars) do
                  {:just, v} -> Nova.Compiler.Ast.expr_case(v, case_clauses)
                  :nothing -> first.body
                end
              _ -> Nova.Compiler.Ast.expr_case((Nova.Compiler.Ast.expr_tuple(param_vars)), (Nova.List.map_maybe((clause_to_tuple_case.(param_vars)), clauses_list)))
            end
            %{name: name, parameters: param_pats, body: case_expr, guards: [], type_signature: first.type_signature}
      end end
      go_merge = Nova.Runtime.fix2(fn go_merge -> fn ds -> fn acc -> case Nova.Array.uncons(ds) do
        :nothing -> Nova.Array.reverse(acc)
        {:just, %{head: d, tail: rest}} -> case d do
            {:decl_function, func} -> 
                %{same_name: same_name, remaining: remaining} = collect_same_name.(func.name).(rest).([])
                all_clauses = Nova.Array.cons(func, same_name)
                if (Nova.Array.length(all_clauses) > 1) do
  go_merge.(remaining).((Nova.Array.cons((Nova.Compiler.Ast.decl_function((merge_clauses_into_one.(all_clauses)))), acc)))
else
  go_merge.(rest).((Nova.Array.cons(d, acc)))
end
            _ -> go_merge.(rest).((Nova.Array.cons(d, acc)))
          end
      end  end end end)
      go_merge.(decls).([])
  end
end
