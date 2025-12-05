defmodule Nova.Compiler.Unify do
  # import Prelude

  # import Data.Either

  # import Data.Map

  # import Data.Set

  # import Data.Array

  # import Data.Foldable

  # import Data.Tuple

  # import Data.Maybe

  # import Nova.Compiler.Types

  # Data type: UnifyError
  def occurs_check(arg0, arg1), do: {:occurs_check, arg0, arg1}
  def type_mismatch(arg0, arg1), do: {:type_mismatch, arg0, arg1}
  def arity_mismatch(arg0, arg1, arg2), do: {:arity_mismatch, arg0, arg1, arg2}
  def record_field_mismatch(arg0), do: {:record_field_mismatch, arg0}

  # instance Show unify_error()



  def show_type(({:ty_var, v})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(v.name, "["), Nova.Runtime.show(v.id)), "]")
  end

  def show_type(({:ty_con, tc})) do
    Nova.Runtime.append(Nova.Runtime.append(Nova.Runtime.append(tc.name, "("), Nova.Runtime.show((Nova.Runtime.length(tc.args)))), " args)")
  end

  def show_type(({:ty_record, r})) do
    "{record}"
  end



  def occurs(v, t) do
    Nova.Set.member(v.id, (Nova.Compiler.Types.free_type_vars(t)))
  end



  def bind_var(v, t) do
    case t do
      {:ty_var, v_prime} when (v.id == v_prime.id) ->
        {:right, Nova.Compiler.Types.empty_subst()}
      _ ->
        cond do
          (occurs(v, t)) ->
            {:left, ({:occurs_check, v, t})}
          (true) ->
            {:right, (Nova.Compiler.Types.single_subst(v, t))}
        end
    end
  end



  def are_equivalent_types(n1, n2) do
    cond do
      ((n1 == n2)) ->
        true
      (((n1 == "List") and (n2 == "Array"))) ->
        true
      (((n1 == "Array") and (n2 == "List"))) ->
        true
      (true) ->
        false
    end
  end



  def is_record_type_alias(name) do
    cond do
      ((name == "TypeClass")) ->
        true
      ((name == "TypeClassInstance")) ->
        true
      ((name == "NewtypeDecl")) ->
        true
      ((name == "FunctionDecl")) ->
        true
      ((name == "FunctionDeclaration")) ->
        true
      ((name == "TypeSig")) ->
        true
      ((name == "DataType")) ->
        true
      ((name == "DataConstructor")) ->
        true
      ((name == "DataField")) ->
        true
      ((name == "TypeAlias")) ->
        true
      ((name == "ModuleDecl")) ->
        true
      ((name == "ModuleExports")) ->
        true
      ((name == "ImportDecl")) ->
        true
      ((name == "InfixDecl")) ->
        true
      ((name == "Constraint")) ->
        true
      ((name == "ForeignImport")) ->
        true
      ((name == "TypeDecl")) ->
        true
      ((name == "LetBind")) ->
        true
      ((name == "CaseClause")) ->
        true
      ((name == "GuardedExpr")) ->
        true
      ((name == "PatResult")) ->
        true
      ((name == "InstantiateResult")) ->
        true
      ((name == "InferResult")) ->
        true
      ((name == "Env")) ->
        true
      ((name == "Scheme")) ->
        true
      ((name == "TVar")) ->
        true
      ((name == "LiftedLambda")) ->
        true
      (true) ->
        false
    end
  end



  def unify(({:ty_var, v}), t) do
    bind_var(v, t)
  end

  def unify(t, ({:ty_var, v})) do
    bind_var(v, t)
  end

  def unify(({:ty_con, c1}), ({:ty_con, c2})) do
    cond do
      (not((are_equivalent_types(c1.name, c2.name)))) ->
        {:left, ({:type_mismatch, ({:ty_con, c1}), ({:ty_con, c2})})}
      ((Nova.Runtime.length(c1.args) != Nova.Runtime.length(c2.args))) ->
        {:left, ({:arity_mismatch, c1.name, (Nova.Runtime.length(c1.args)), (Nova.Runtime.length(c2.args))})}
      (true) ->
        unify_many(c1.args, c2.args)
    end
  end

  def unify(({:ty_con, c}), ({:ty_record, r})) do
    cond do
      (((Nova.Runtime.length(c.args) == 0) and is_record_type_alias(c.name))) ->
        {:right, Nova.Compiler.Types.empty_subst()}
      (true) ->
        {:left, ({:type_mismatch, ({:ty_con, c}), ({:ty_record, r})})}
    end
  end

  def unify(({:ty_record, r}), ({:ty_con, c})) do
    cond do
      (((Nova.Runtime.length(c.args) == 0) and is_record_type_alias(c.name))) ->
        {:right, Nova.Compiler.Types.empty_subst()}
      (true) ->
        {:left, ({:type_mismatch, ({:ty_record, r}), ({:ty_con, c})})}
    end
  end

  def unify(({:ty_record, r1}), ({:ty_record, r2})) do
    unify_records(r1, r2)
  end

  def unify(t1, t2) do
    {:left, ({:type_mismatch, t1, t2})}
  end



  def unify_step(sub, ({:tuple, t1, t2})) do
      Nova.Runtime.bind(unify((Nova.Compiler.Types.apply_subst(sub, t1)), (Nova.Compiler.Types.apply_subst(sub, t2))), fn s ->
    Nova.Runtime.pure((Nova.Compiler.Types.compose_subst(s, sub)))
  end)
  end



  def unify_many(ts1, ts2) do
    Nova.Runtime.fold_m((&unify_step/2), Nova.Compiler.Types.empty_subst(), (Nova.Runtime.zip(ts1, ts2)))
  end



  def unify_field(fields1, fields2, sub, k) do
    case {:tuple, (Nova.Map.lookup(k, fields1)), (Nova.Map.lookup(k, fields2))} do
      {:tuple, ({:just, t1}), ({:just, t2})} -> case unify((Nova.Compiler.Types.apply_subst(sub, t1)), (Nova.Compiler.Types.apply_subst(sub, t2))) do
          {:left, err} -> {:left, err}
          {:right, s} -> {:right, (Nova.Compiler.Types.compose_subst(s, sub))}
        end
      _ -> {:right, sub}
    end
  end



  def unify_records(r1, r2) do
    
      keys1 = Nova.Map.keys(r1.fields)
      Nova.Runtime.fold_m((fn auto_p0 -> fn auto_p1 -> unify_field(r1.fields, r2.fields, auto_p0, auto_p1) end end), Nova.Compiler.Types.empty_subst(), keys1)
  end
end
