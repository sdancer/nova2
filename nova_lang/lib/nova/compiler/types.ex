defmodule Nova.Compiler.Types do
  # import Prelude

  # import Data.Map

  # import Data.Map

  # import Data.Set

  # import Data.Set

  # import Data.Maybe

  # import Data.Tuple

  # import Data.Foldable

  # import Data.Array

  # import Data.String

  # import Nova.Compiler.Ast

  # @type tvar :: %{id: int(), name: string()}

  def mk_tvar(id, name) do
    %{id: id, name: name}
  end

  # @type tcon :: %{name: string(), args: array()(type())}

  def normalize_type_name(name) do
    case Nova.String.last_index_of(Nova.String.pattern("."), name) do
      :nothing -> name
      {:just, idx} -> Nova.String.drop((idx + 1), name)
    end
  end

  def mk_tcon(name, args) do
    %{name: normalize_type_name(name), args: args}
  end

  def mk_tcon0(name) do
    %{name: normalize_type_name(name), args: []}
  end

  # @type record :: %{fields: map()(string())(type()), row: maybe()(tvar())}

  # Data type: Type
  def ty_var(arg0), do: {:ty_var, arg0}
  def ty_con(arg0), do: {:ty_con, arg0}
  def ty_record(arg0), do: {:ty_record, arg0}

  # unsupported declaration

  def t_int() do
    {:ty_con, mk_tcon0("Int")}
  end

  def t_string() do
    {:ty_con, mk_tcon0("String")}
  end

  def t_char() do
    {:ty_con, mk_tcon0("Char")}
  end

  def t_bool() do
    {:ty_con, mk_tcon0("Bool")}
  end

  def t_list(el) do
    {:ty_con, mk_tcon("List", [el])}
  end

  def t_array(el) do
    {:ty_con, mk_tcon("Array", [el])}
  end

  def t_arrow(a, b) do
    {:ty_con, mk_tcon("Fun", [a, b])}
  end

  def t_maybe(t) do
    {:ty_con, mk_tcon("Maybe", [t])}
  end

  def t_either(l, r) do
    {:ty_con, mk_tcon("Either", [l, r])}
  end

  def t_map(k, v) do
    {:ty_con, mk_tcon("Map", [k, v])}
  end

  def t_set(t) do
    {:ty_con, mk_tcon("Set", [t])}
  end

  def t_unit() do
    {:ty_con, mk_tcon0("Unit")}
  end

  def t_number() do
    {:ty_con, mk_tcon0("Number")}
  end

  def t_token_type() do
    {:ty_con, mk_tcon0("TokenType")}
  end

  def t_tuple(ts) do
    
      n = Nova.Array.length(ts)
      name = if (n == 2) do
        "Tuple"
      else
        Nova.Runtime.append("Tuple", Nova.Runtime.show(n))
      end
      {:ty_con, %{name: name, args: ts}}
  end

  # @type subst :: map()(int())(type())

  def empty_subst() do
    Nova.Map.empty
  end

  def lookup_subst(sub, v) do
    case Nova.Map.lookup(v.id, sub) do
      {:just, t} -> t
      :nothing -> {:ty_var, v}
    end
  end

  def single_subst(v, t) do
    Nova.Map.singleton(v.id, t)
  end

  def apply_subst(sub, ({:ty_var, v})) do
    lookup_subst(sub, v)
  end

  def apply_subst(sub, ({:ty_con, c})) do
    {:ty_con, %{c | args: Nova.Runtime.map(fn __p0__ -> apply_subst(sub, __p0__) end, c.args)}}
  end

  def apply_subst(sub, ({:ty_record, r})) do
    {:ty_record, %{r | fields: Nova.Runtime.map(fn __p0__ -> apply_subst(sub, __p0__) end, r.fields)}}
  end

  def compose_subst(s1, s2) do
    Nova.Map.union(s1, Nova.Runtime.map(fn __p0__ -> apply_subst(s1, __p0__) end, s2))
  end

  # @type scheme :: %{vars: array()(tvar()), ty: type()}

  def mk_scheme(vars, ty) do
    %{vars: vars, ty: ty}
  end

  def free_type_vars(({:ty_var, v})) do
    Nova.Set.singleton(v.id)
  end

  def free_type_vars(({:ty_con, c})) do
    Nova.Runtime.foldl(fn acc -> fn t -> Nova.Set.union(acc, free_type_vars(t)) end end, Nova.Set.empty, c.args)
  end

  def free_type_vars(({:ty_record, r})) do
    Nova.Runtime.foldl(fn acc -> fn t -> Nova.Set.union(acc, free_type_vars(t)) end end, Nova.Set.empty, r.fields)
  end

  def free_type_vars_scheme(s) do
    
      bound_ids = Nova.Set.from_foldable(Nova.Runtime.map(& &1.id, s.vars))
      Nova.Set.difference(free_type_vars(s.ty), bound_ids)
  end

  # @type env :: %{bindings: map()(string())(scheme()), counter: int(), registry_layer: maybe()(int()), namespace: maybe()(string())}

  def empty_env() do
    %{bindings: builtin_prelude(), counter: 0, registry_layer: :nothing, namespace: :nothing}
  end

  def extend_env(env, name, scheme) do
    %{env | bindings: Nova.Map.insert(name, scheme, env.bindings)}
  end

  def lookup_env(env, name) do
    Nova.Map.lookup(name, env.bindings)
  end

  def apply_subst_to_env(sub, env) do
    
      apply_to_scheme = fn s -> %{s | ty: apply_subst(sub, s.ty)} end
      %{env | bindings: Nova.Runtime.map(apply_to_scheme, env.bindings)}
  end

  def fresh_var(env, hint) do
    
      v = mk_tvar(env.counter, Nova.Runtime.append(hint, Nova.Runtime.show(env.counter)))
      env_prime = %{env | counter: (env.counter + 1)}
      {:tuple, v, env_prime}
  end

  def free_type_vars_env(env) do
    Nova.Runtime.foldl(fn acc -> fn s -> Nova.Set.union(acc, free_type_vars_scheme(s)) end end, Nova.Set.empty, env.bindings)
  end

  def builtin_prelude() do
    
      a = mk_tvar(-1, "a")
      b = mk_tvar(-2, "b")
      c = mk_tvar(-3, "c")
      d = mk_tvar(-6, "d")
      e = mk_tvar(-7, "e")
      k = mk_tvar(-4, "k")
      v = mk_tvar(-5, "v")
      Nova.Map.from_foldable([{:tuple, "Int", mk_scheme([], t_int())}, {:tuple, "String", mk_scheme([], t_string())}, {:tuple, "Char", mk_scheme([], t_char())}, {:tuple, "Bool", mk_scheme([], t_bool())}, {:tuple, "Boolean", mk_scheme([], t_bool())}, {:tuple, "True", mk_scheme([], t_bool())}, {:tuple, "False", mk_scheme([], t_bool())}, {:tuple, "Array", mk_scheme([a], t_array({:ty_var, a}))}, {:tuple, "List", mk_scheme([a], t_list({:ty_var, a}))}, {:tuple, "List.fromFoldable", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_list({:ty_var, a})))}, {:tuple, "Array.fromFoldable", mk_scheme([a], t_arrow(t_list({:ty_var, a}), t_array({:ty_var, a})))}, {:tuple, "Array.toUnfoldable", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_list({:ty_var, a})))}, {:tuple, "Cons", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow(t_list({:ty_var, a}), t_list({:ty_var, a}))))}, {:tuple, "Nil", mk_scheme([a], t_list({:ty_var, a}))}, {:tuple, "Maybe", mk_scheme([a], {:ty_con, mk_tcon("Maybe", [{:ty_var, a}])})}, {:tuple, "Either", mk_scheme([a, b], {:ty_con, mk_tcon("Either", [{:ty_var, a}, {:ty_var, b}])})}, {:tuple, "+", mk_scheme([], t_arrow(t_int(), t_arrow(t_int(), t_int())))}, {:tuple, "-", mk_scheme([], t_arrow(t_int(), t_arrow(t_int(), t_int())))}, {:tuple, "*", mk_scheme([], t_arrow(t_int(), t_arrow(t_int(), t_int())))}, {:tuple, "/", mk_scheme([], t_arrow(t_int(), t_arrow(t_int(), t_int())))}, {:tuple, "mod", mk_scheme([], t_arrow(t_int(), t_arrow(t_int(), t_int())))}, {:tuple, "negate", mk_scheme([], t_arrow(t_int(), t_int()))}, {:tuple, "<", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow({:ty_var, a}, t_bool())))}, {:tuple, ">", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow({:ty_var, a}, t_bool())))}, {:tuple, "<=", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow({:ty_var, a}, t_bool())))}, {:tuple, ">=", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow({:ty_var, a}, t_bool())))}, {:tuple, "==", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow({:ty_var, a}, t_bool())))}, {:tuple, "/=", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow({:ty_var, a}, t_bool())))}, {:tuple, "&&", mk_scheme([], t_arrow(t_bool(), t_arrow(t_bool(), t_bool())))}, {:tuple, "||", mk_scheme([], t_arrow(t_bool(), t_arrow(t_bool(), t_bool())))}, {:tuple, "not", mk_scheme([], t_arrow(t_bool(), t_bool()))}, {:tuple, "otherwise", mk_scheme([], t_bool())}, {:tuple, "<>", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow({:ty_var, a}, {:ty_var, a})))}, {:tuple, ":", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow(t_array({:ty_var, a}), t_array({:ty_var, a}))))}, {:tuple, "++", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_arrow(t_array({:ty_var, a}), t_array({:ty_var, a}))))}, {:tuple, "$", mk_scheme([a, b], t_arrow(t_arrow({:ty_var, a}, {:ty_var, b}), t_arrow({:ty_var, a}, {:ty_var, b})))}, {:tuple, ".", mk_scheme([a, b, c], t_arrow(t_arrow({:ty_var, b}, {:ty_var, c}), t_arrow(t_arrow({:ty_var, a}, {:ty_var, b}), t_arrow({:ty_var, a}, {:ty_var, c}))))}, {:tuple, "identity", mk_scheme([a], t_arrow({:ty_var, a}, {:ty_var, a}))}, {:tuple, "const", mk_scheme([a, b], t_arrow({:ty_var, a}, t_arrow({:ty_var, b}, {:ty_var, a})))}, {:tuple, "#", mk_scheme([a, b], t_arrow({:ty_var, a}, t_arrow(t_arrow({:ty_var, a}, {:ty_var, b}), {:ty_var, b})))}, {:tuple, "Array.head", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_maybe({:ty_var, a})))}, {:tuple, "Array.last", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_maybe({:ty_var, a})))}, {:tuple, "Array.tail", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_maybe(t_array({:ty_var, a}))))}, {:tuple, "Array.init", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_maybe(t_array({:ty_var, a}))))}, {:tuple, "Array.uncons", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_maybe({:ty_var, a})))}, {:tuple, "Array.length", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_int()))}, {:tuple, "Array.null", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_bool()))}, {:tuple, "Array.elem", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow(t_array({:ty_var, a}), t_bool())))}, {:tuple, "Array.cons", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow(t_array({:ty_var, a}), t_array({:ty_var, a}))))}, {:tuple, "Array.snoc", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_arrow({:ty_var, a}, t_array({:ty_var, a}))))}, {:tuple, "Array.take", mk_scheme([a], t_arrow(t_int(), t_arrow(t_array({:ty_var, a}), t_array({:ty_var, a}))))}, {:tuple, "Array.drop", mk_scheme([a], t_arrow(t_int(), t_arrow(t_array({:ty_var, a}), t_array({:ty_var, a}))))}, {:tuple, "Array.reverse", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_array({:ty_var, a})))}, {:tuple, "Array.filter", mk_scheme([a], t_arrow(t_arrow({:ty_var, a}, t_bool()), t_arrow(t_array({:ty_var, a}), t_array({:ty_var, a}))))}, {:tuple, "Array.find", mk_scheme([a], t_arrow(t_arrow({:ty_var, a}, t_bool()), t_arrow(t_array({:ty_var, a}), t_maybe({:ty_var, a}))))}, {:tuple, "Array.foldl", mk_scheme([a, b], t_arrow(t_arrow({:ty_var, b}, t_arrow({:ty_var, a}, {:ty_var, b})), t_arrow({:ty_var, b}, t_arrow(t_array({:ty_var, a}), {:ty_var, b}))))}, {:tuple, "Array.foldr", mk_scheme([a, b], t_arrow(t_arrow({:ty_var, a}, t_arrow({:ty_var, b}, {:ty_var, b})), t_arrow({:ty_var, b}, t_arrow(t_array({:ty_var, a}), {:ty_var, b}))))}, {:tuple, "Array.map", mk_scheme([a, b], t_arrow(t_arrow({:ty_var, a}, {:ty_var, b}), t_arrow(t_array({:ty_var, a}), t_array({:ty_var, b}))))}, {:tuple, "Array.mapWithIndex", mk_scheme([a, b], t_arrow(t_arrow(t_int(), t_arrow({:ty_var, a}, {:ty_var, b})), t_arrow(t_array({:ty_var, a}), t_array({:ty_var, b}))))}, {:tuple, "mapWithIndex", mk_scheme([a, b], t_arrow(t_arrow(t_int(), t_arrow({:ty_var, a}, {:ty_var, b})), t_arrow(t_array({:ty_var, a}), t_array({:ty_var, b}))))}, {:tuple, "Array.replicate", mk_scheme([a], t_arrow(t_int(), t_arrow({:ty_var, a}, t_array({:ty_var, a}))))}, {:tuple, "Array.zip", mk_scheme([a, b], t_arrow(t_array({:ty_var, a}), t_arrow(t_array({:ty_var, b}), t_array(t_tuple([{:ty_var, a}, {:ty_var, b}])))))}, {:tuple, "Array.dropWhile", mk_scheme([a], t_arrow(t_arrow({:ty_var, a}, t_bool()), t_arrow(t_array({:ty_var, a}), t_array({:ty_var, a}))))}, {:tuple, "Array.span", mk_scheme([a], t_arrow(t_arrow({:ty_var, a}, t_bool()), t_arrow(t_array({:ty_var, a}), {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "init", t_array({:ty_var, a})}, {:tuple, "rest", t_array({:ty_var, a})}]), row: :nothing}})))}, {:tuple, "Array.mapMaybe", mk_scheme([a, b], t_arrow(t_arrow({:ty_var, a}, t_maybe({:ty_var, b})), t_arrow(t_array({:ty_var, a}), t_array({:ty_var, b}))))}, {:tuple, "Array.range", mk_scheme([], t_arrow(t_int(), t_arrow(t_int(), t_array(t_int()))))}, {:tuple, "Array.any", mk_scheme([a], t_arrow(t_arrow({:ty_var, a}, t_bool()), t_arrow(t_array({:ty_var, a}), t_bool())))}, {:tuple, "Array.all", mk_scheme([a], t_arrow(t_arrow({:ty_var, a}, t_bool()), t_arrow(t_array({:ty_var, a}), t_bool())))}, {:tuple, "Array.concatMap", mk_scheme([a, b], t_arrow(t_arrow({:ty_var, a}, t_array({:ty_var, b})), t_arrow(t_array({:ty_var, a}), t_array({:ty_var, b}))))}, {:tuple, "Array.concat", mk_scheme([a], t_arrow(t_array(t_array({:ty_var, a})), t_array({:ty_var, a})))}, {:tuple, "Array.partition", mk_scheme([a], t_arrow(t_arrow({:ty_var, a}, t_bool()), t_arrow(t_array({:ty_var, a}), {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "yes", t_array({:ty_var, a})}, {:tuple, "no", t_array({:ty_var, a})}]), row: :nothing}})))}, {:tuple, "Array.mapWithIndex", mk_scheme([a, b], t_arrow(t_arrow(t_int(), t_arrow({:ty_var, a}, {:ty_var, b})), t_arrow(t_array({:ty_var, a}), t_array({:ty_var, b}))))}, {:tuple, "charLt", mk_scheme([], t_arrow(t_char(), t_arrow(t_char(), t_bool())))}, {:tuple, "charGt", mk_scheme([], t_arrow(t_char(), t_arrow(t_char(), t_bool())))}, {:tuple, "charLte", mk_scheme([], t_arrow(t_char(), t_arrow(t_char(), t_bool())))}, {:tuple, "charGte", mk_scheme([], t_arrow(t_char(), t_arrow(t_char(), t_bool())))}, {:tuple, "charEq", mk_scheme([], t_arrow(t_char(), t_arrow(t_char(), t_bool())))}, {:tuple, "isAlpha", mk_scheme([], t_arrow(t_char(), t_bool()))}, {:tuple, "isDigit", mk_scheme([], t_arrow(t_char(), t_bool()))}, {:tuple, "isAlphaNum", mk_scheme([], t_arrow(t_char(), t_bool()))}, {:tuple, "isSpace", mk_scheme([], t_arrow(t_char(), t_bool()))}, {:tuple, "isUpper", mk_scheme([], t_arrow(t_char(), t_bool()))}, {:tuple, "isLower", mk_scheme([], t_arrow(t_char(), t_bool()))}, {:tuple, "CU.charAt", mk_scheme([], t_arrow(t_int(), t_arrow(t_string(), t_maybe(t_char()))))}, {:tuple, "CU.length", mk_scheme([], t_arrow(t_string(), t_int()))}, {:tuple, "CU.drop", mk_scheme([], t_arrow(t_int(), t_arrow(t_string(), t_string())))}, {:tuple, "CU.take", mk_scheme([], t_arrow(t_int(), t_arrow(t_string(), t_string())))}, {:tuple, "CU.singleton", mk_scheme([], t_arrow(t_char(), t_string()))}, {:tuple, "CU.toCharArray", mk_scheme([], t_arrow(t_string(), t_array(t_char())))}, {:tuple, "CU.fromCharArray", mk_scheme([], t_arrow(t_array(t_char()), t_string()))}, {:tuple, "SCU.charAt", mk_scheme([], t_arrow(t_int(), t_arrow(t_string(), t_maybe(t_char()))))}, {:tuple, "SCU.length", mk_scheme([], t_arrow(t_string(), t_int()))}, {:tuple, "SCU.drop", mk_scheme([], t_arrow(t_int(), t_arrow(t_string(), t_string())))}, {:tuple, "SCU.take", mk_scheme([], t_arrow(t_int(), t_arrow(t_string(), t_string())))}, {:tuple, "SCU.singleton", mk_scheme([], t_arrow(t_char(), t_string()))}, {:tuple, "SCU.toCharArray", mk_scheme([], t_arrow(t_string(), t_array(t_char())))}, {:tuple, "SCU.fromCharArray", mk_scheme([], t_arrow(t_array(t_char()), t_string()))}, {:tuple, "String.length", mk_scheme([], t_arrow(t_string(), t_int()))}, {:tuple, "String.take", mk_scheme([], t_arrow(t_int(), t_arrow(t_string(), t_string())))}, {:tuple, "String.drop", mk_scheme([], t_arrow(t_int(), t_arrow(t_string(), t_string())))}, {:tuple, "String.joinWith", mk_scheme([], t_arrow(t_string(), t_arrow(t_array(t_string()), t_string())))}, {:tuple, "String.singleton", mk_scheme([], t_arrow(t_char(), t_string()))}, {:tuple, "String.toCodePointArray", mk_scheme([], t_arrow(t_string(), t_array(t_char())))}, {:tuple, "String.toLower", mk_scheme([], t_arrow(t_string(), t_string()))}, {:tuple, "String.toUpper", mk_scheme([], t_arrow(t_string(), t_string()))}, {:tuple, "String.contains", mk_scheme([], t_arrow(t_string(), t_arrow(t_string(), t_bool())))}, {:tuple, "String.replaceAll", mk_scheme([], t_arrow(t_string(), t_arrow(t_string(), t_arrow(t_string(), t_string()))))}, {:tuple, "String.Pattern", mk_scheme([], t_arrow(t_string(), t_string()))}, {:tuple, "String.split", mk_scheme([], t_arrow(t_string(), t_arrow(t_string(), t_array(t_string()))))}, {:tuple, "String.stripPrefix", mk_scheme([], t_arrow(t_string(), t_arrow(t_string(), t_maybe(t_string()))))}, {:tuple, "String.indexOf", mk_scheme([], t_arrow(t_string(), t_arrow(t_string(), t_maybe(t_int()))))}, {:tuple, "String.lastIndexOf", mk_scheme([], t_arrow(t_string(), t_arrow(t_string(), t_maybe(t_int()))))}, {:tuple, "String.Replacement", mk_scheme([], t_arrow(t_string(), t_string()))}, {:tuple, "Just", mk_scheme([a], t_arrow({:ty_var, a}, t_maybe({:ty_var, a})))}, {:tuple, "Nothing", mk_scheme([a], t_maybe({:ty_var, a}))}, {:tuple, "maybe", mk_scheme([a, b], t_arrow({:ty_var, b}, t_arrow(t_arrow({:ty_var, a}, {:ty_var, b}), t_arrow(t_maybe({:ty_var, a}), {:ty_var, b}))))}, {:tuple, "fromMaybe", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow(t_maybe({:ty_var, a}), {:ty_var, a})))}, {:tuple, "isJust", mk_scheme([a], t_arrow(t_maybe({:ty_var, a}), t_bool()))}, {:tuple, "isNothing", mk_scheme([a], t_arrow(t_maybe({:ty_var, a}), t_bool()))}, {:tuple, "Left", mk_scheme([a, b], t_arrow({:ty_var, a}, t_either({:ty_var, a}, {:ty_var, b})))}, {:tuple, "Right", mk_scheme([a, b], t_arrow({:ty_var, b}, t_either({:ty_var, a}, {:ty_var, b})))}, {:tuple, "either", mk_scheme([a, b, c], t_arrow(t_arrow({:ty_var, a}, {:ty_var, c}), t_arrow(t_arrow({:ty_var, b}, {:ty_var, c}), t_arrow(t_either({:ty_var, a}, {:ty_var, b}), {:ty_var, c}))))}, {:tuple, "pure", mk_scheme([a, b], t_arrow({:ty_var, a}, {:ty_var, b}))}, {:tuple, "Map.empty", mk_scheme([k, v], t_map({:ty_var, k}, {:ty_var, v}))}, {:tuple, "Map.singleton", mk_scheme([k, v], t_arrow({:ty_var, k}, t_arrow({:ty_var, v}, t_map({:ty_var, k}, {:ty_var, v}))))}, {:tuple, "Map.insert", mk_scheme([k, v], t_arrow({:ty_var, k}, t_arrow({:ty_var, v}, t_arrow(t_map({:ty_var, k}, {:ty_var, v}), t_map({:ty_var, k}, {:ty_var, v})))))}, {:tuple, "Map.lookup", mk_scheme([k, v], t_arrow({:ty_var, k}, t_arrow(t_map({:ty_var, k}, {:ty_var, v}), t_maybe({:ty_var, v}))))}, {:tuple, "Map.member", mk_scheme([k, v], t_arrow({:ty_var, k}, t_arrow(t_map({:ty_var, k}, {:ty_var, v}), t_bool())))}, {:tuple, "Map.keys", mk_scheme([k, v], t_arrow(t_map({:ty_var, k}, {:ty_var, v}), t_array({:ty_var, k})))}, {:tuple, "Map.values", mk_scheme([k, v], t_arrow(t_map({:ty_var, k}, {:ty_var, v}), t_array({:ty_var, v})))}, {:tuple, "Map.union", mk_scheme([k, v], t_arrow(t_map({:ty_var, k}, {:ty_var, v}), t_arrow(t_map({:ty_var, k}, {:ty_var, v}), t_map({:ty_var, k}, {:ty_var, v}))))}, {:tuple, "Map.fromFoldable", mk_scheme([k, v], t_arrow(t_array(t_tuple([{:ty_var, k}, {:ty_var, v}])), t_map({:ty_var, k}, {:ty_var, v})))}, {:tuple, "Set.empty", mk_scheme([a], t_set({:ty_var, a}))}, {:tuple, "Set.singleton", mk_scheme([a], t_arrow({:ty_var, a}, t_set({:ty_var, a})))}, {:tuple, "Set.insert", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow(t_set({:ty_var, a}), t_set({:ty_var, a}))))}, {:tuple, "Set.member", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow(t_set({:ty_var, a}), t_bool())))}, {:tuple, "Set.delete", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow(t_set({:ty_var, a}), t_set({:ty_var, a}))))}, {:tuple, "Set.union", mk_scheme([a], t_arrow(t_set({:ty_var, a}), t_arrow(t_set({:ty_var, a}), t_set({:ty_var, a}))))}, {:tuple, "Set.difference", mk_scheme([a], t_arrow(t_set({:ty_var, a}), t_arrow(t_set({:ty_var, a}), t_set({:ty_var, a}))))}, {:tuple, "Set.fromFoldable", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_set({:ty_var, a})))}, {:tuple, "Set.toUnfoldable", mk_scheme([a], t_arrow(t_set({:ty_var, a}), t_array({:ty_var, a})))}, {:tuple, "foldl", mk_scheme([a, b, c], t_arrow(t_arrow({:ty_var, b}, t_arrow({:ty_var, a}, {:ty_var, b})), t_arrow({:ty_var, b}, t_arrow({:ty_var, c}, {:ty_var, b}))))}, {:tuple, "foldr", mk_scheme([a, b, c], t_arrow(t_arrow({:ty_var, a}, t_arrow({:ty_var, b}, {:ty_var, b})), t_arrow({:ty_var, b}, t_arrow({:ty_var, c}, {:ty_var, b}))))}, {:tuple, "foldM", mk_scheme([a, b, c, d], t_arrow(t_arrow({:ty_var, b}, t_arrow({:ty_var, a}, {:ty_var, d})), t_arrow({:ty_var, b}, t_arrow({:ty_var, c}, {:ty_var, d}))))}, {:tuple, "Tuple", mk_scheme([a, b], t_arrow({:ty_var, a}, t_arrow({:ty_var, b}, t_tuple([{:ty_var, a}, {:ty_var, b}]))))}, {:tuple, "Tuple2", mk_scheme([a, b], t_arrow({:ty_var, a}, t_arrow({:ty_var, b}, t_tuple([{:ty_var, a}, {:ty_var, b}]))))}, {:tuple, "Tuple3", mk_scheme([a, b, c], t_arrow({:ty_var, a}, t_arrow({:ty_var, b}, t_arrow({:ty_var, c}, t_tuple([{:ty_var, a}, {:ty_var, b}, {:ty_var, c}])))))}, {:tuple, "Tuple4", mk_scheme([a, b, c, d], t_arrow({:ty_var, a}, t_arrow({:ty_var, b}, t_arrow({:ty_var, c}, t_arrow({:ty_var, d}, t_tuple([{:ty_var, a}, {:ty_var, b}, {:ty_var, c}, {:ty_var, d}]))))))}, {:tuple, "Tuple5", mk_scheme([a, b, c, d, e], t_arrow({:ty_var, a}, t_arrow({:ty_var, b}, t_arrow({:ty_var, c}, t_arrow({:ty_var, d}, t_arrow({:ty_var, e}, t_tuple([{:ty_var, a}, {:ty_var, b}, {:ty_var, c}, {:ty_var, d}, {:ty_var, e}])))))))}, {:tuple, "fst", mk_scheme([a, b], t_arrow(t_tuple([{:ty_var, a}, {:ty_var, b}]), {:ty_var, a}))}, {:tuple, "snd", mk_scheme([a, b], t_arrow(t_tuple([{:ty_var, a}, {:ty_var, b}]), {:ty_var, b}))}, {:tuple, "show", mk_scheme([a], t_arrow({:ty_var, a}, t_string()))}, {:tuple, "TokKeyword", mk_scheme([], t_token_type())}, {:tuple, "TokIdentifier", mk_scheme([], t_token_type())}, {:tuple, "TokNumber", mk_scheme([], t_token_type())}, {:tuple, "TokString", mk_scheme([], t_token_type())}, {:tuple, "TokChar", mk_scheme([], t_token_type())}, {:tuple, "TokOperator", mk_scheme([], t_token_type())}, {:tuple, "TokDelimiter", mk_scheme([], t_token_type())}, {:tuple, "TokNewline", mk_scheme([], t_token_type())}, {:tuple, "TokUnrecognized", mk_scheme([], t_token_type())}, {:tuple, "TyVar", mk_scheme([], t_arrow(t_tvar(), t_type()))}, {:tuple, "TyCon", mk_scheme([], t_arrow(t_tcon(), t_type()))}, {:tuple, "TyRecord", mk_scheme([], t_arrow(t_record(), t_type()))}, {:tuple, "Int.fromString", mk_scheme([], t_arrow(t_string(), t_maybe(t_int())))}, {:tuple, "Number.fromString", mk_scheme([], t_arrow(t_string(), t_maybe(t_number())))}, {:tuple, "pure", mk_scheme([a], t_arrow({:ty_var, a}, {:ty_var, a}))}, {:tuple, "map", mk_scheme([a, b, c, d], t_arrow(t_arrow({:ty_var, a}, {:ty_var, b}), t_arrow({:ty_var, c}, {:ty_var, d})))}, {:tuple, "intercalate", mk_scheme([a], t_arrow({:ty_var, a}, t_arrow(t_array({:ty_var, a}), {:ty_var, a})))}, {:tuple, "length", mk_scheme([a], t_arrow(t_array({:ty_var, a}), t_int()))}, {:tuple, "zip", mk_scheme([a, b], t_arrow(t_array({:ty_var, a}), t_arrow(t_array({:ty_var, b}), t_array({:ty_var, a}))))}, {:tuple, "applySubst", mk_scheme([a], t_arrow(t_subst(), t_arrow({:ty_var, a}, {:ty_var, a})))}, {:tuple, "freeTypeVars", mk_scheme([a], t_arrow({:ty_var, a}, t_set(t_int())))}, {:tuple, "freeTypeVarsScheme", mk_scheme([], t_arrow(t_scheme(), t_set(t_int())))}, {:tuple, "freeTypeVarsEnv", mk_scheme([], t_arrow(t_env(), t_set(t_int())))}, {:tuple, "composeSubst", mk_scheme([], t_arrow(t_subst(), t_arrow(t_subst(), t_subst())))}, {:tuple, "emptySubst", mk_scheme([], t_subst())}, {:tuple, "lookupSubst", mk_scheme([], t_arrow(t_subst(), t_arrow(t_tvar(), t_type())))}, {:tuple, "extendEnv", mk_scheme([], t_arrow(t_env(), t_arrow(t_string(), t_arrow(t_scheme(), t_env()))))}, {:tuple, "applySubstToEnv", mk_scheme([], t_arrow(t_subst(), t_arrow(t_env(), t_env())))}, {:tuple, "lookupEnv", mk_scheme([], t_arrow(t_env(), t_arrow(t_string(), t_maybe(t_scheme()))))}, {:tuple, "freshVar", mk_scheme([], t_arrow(t_env(), t_arrow(t_string(), t_tuple([t_tvar(), t_env()]))))}, {:tuple, "mkScheme", mk_scheme([], t_arrow(t_array(t_tvar()), t_arrow(t_type(), t_scheme())))}, {:tuple, "mkTVar", mk_scheme([], t_arrow(t_int(), t_arrow(t_string(), t_tvar())))}, {:tuple, "mkTCon", mk_scheme([], t_arrow(t_string(), t_arrow(t_array(t_type()), t_tcon())))}, {:tuple, "mkTCon0", mk_scheme([], t_arrow(t_string(), t_tcon()))}, {:tuple, "generalize", mk_scheme([], t_arrow(t_env(), t_arrow(t_type(), t_scheme())))}, {:tuple, "builtinPrelude", mk_scheme([], t_map(t_string(), t_scheme()))}, {:tuple, "singleSubst", mk_scheme([], t_arrow(t_tvar(), t_arrow(t_type(), t_subst())))}, {:tuple, "tInt", mk_scheme([], t_type())}, {:tuple, "tString", mk_scheme([], t_type())}, {:tuple, "tChar", mk_scheme([], t_type())}, {:tuple, "tBool", mk_scheme([], t_type())}, {:tuple, "tArray", mk_scheme([], t_arrow(t_type(), t_type()))}, {:tuple, "tArrow", mk_scheme([], t_arrow(t_type(), t_arrow(t_type(), t_type())))}, {:tuple, "tMaybe", mk_scheme([], t_arrow(t_type(), t_type()))}, {:tuple, "tEither", mk_scheme([], t_arrow(t_type(), t_arrow(t_type(), t_type())))}, {:tuple, "tTuple", mk_scheme([], t_arrow(t_array(t_type()), t_type()))}, {:tuple, "tMap", mk_scheme([], t_arrow(t_type(), t_arrow(t_type(), t_type())))}, {:tuple, "tSet", mk_scheme([], t_arrow(t_type(), t_type()))}, {:tuple, "tList", mk_scheme([], t_arrow(t_type(), t_type()))}, {:tuple, "tNumber", mk_scheme([], t_type())}, {:tuple, "unify", mk_scheme([], t_arrow(t_type(), t_arrow(t_type(), t_either(t_unify_error(), t_subst()))))}, {:tuple, "unifyMany", mk_scheme([], t_arrow(t_array(t_type()), t_arrow(t_array(t_type()), t_either(t_unify_error(), t_subst()))))}, {:tuple, "bindVar", mk_scheme([], t_arrow(t_tvar(), t_arrow(t_type(), t_either(t_unify_error(), t_subst()))))}, {:tuple, "occurs", mk_scheme([], t_arrow(t_tvar(), t_arrow(t_type(), t_bool())))}, {:tuple, "unifyRecords", mk_scheme([], t_arrow(t_record(), t_arrow(t_record(), t_either(t_unify_error(), t_subst()))))}, {:tuple, "OccursCheck", mk_scheme([], t_arrow(t_tvar(), t_arrow(t_type(), t_unify_error())))}, {:tuple, "TypeMismatch", mk_scheme([], t_arrow(t_type(), t_arrow(t_type(), t_unify_error())))}, {:tuple, "ArityMismatch", mk_scheme([], t_arrow(t_string(), t_arrow(t_int(), t_arrow(t_int(), t_unify_error()))))}, {:tuple, "RecordFieldMismatch", mk_scheme([], t_arrow(t_string(), t_unify_error()))}, {:tuple, "UnifyErr", mk_scheme([], t_arrow(t_unify_error(), t_tcerror()))}, {:tuple, "UnboundVariable", mk_scheme([], t_arrow(t_string(), t_tcerror()))}, {:tuple, "NotImplemented", mk_scheme([], t_arrow(t_string(), t_tcerror()))}, {:tuple, "inferLit", mk_scheme([], t_arrow(t_literal(), t_type()))}, {:tuple, "inferPat", mk_scheme([], t_arrow(t_env(), t_arrow(t_pattern(), t_arrow(t_type(), t_either(t_tcerror(), t_pat_result())))))}, {:tuple, "instantiate", mk_scheme([], t_arrow(t_env(), t_arrow(t_scheme(), t_instantiate_result())))}, {:tuple, "infer", mk_scheme([], t_arrow(t_env(), t_arrow(t_expr(), t_either(t_tcerror(), t_infer_result()))))}, {:tuple, "generalize", mk_scheme([], t_arrow(t_env(), t_arrow(t_type(), t_scheme())))}, {:tuple, "checkDecl", mk_scheme([], t_arrow(t_env(), t_arrow(t_declaration(), t_either(t_tcerror(), t_env()))))}, {:tuple, "checkModule", mk_scheme([], t_arrow(t_env(), t_arrow(t_array(t_declaration()), t_either(t_tcerror(), t_env()))))}, {:tuple, "inferDo", mk_scheme([], t_arrow(t_env(), t_arrow(t_array(t_do_statement()), t_either(t_tcerror(), t_infer_result()))))}, {:tuple, "inferRecordUpdate", mk_scheme([], t_arrow(t_env(), t_arrow(t_expr(), t_arrow(t_array(t_tuple([t_string(), t_expr()])), t_either(t_tcerror(), t_infer_result())))))}, {:tuple, "inferUnaryOp", mk_scheme([], t_arrow(t_env(), t_arrow(t_string(), t_arrow(t_expr(), t_either(t_tcerror(), t_infer_result())))))}, {:tuple, "TyVar", mk_scheme([], t_arrow(t_tvar(), t_type()))}, {:tuple, "TyCon", mk_scheme([], t_arrow(t_tcon_record(), t_type()))}, {:tuple, "TyRecord", mk_scheme([], t_arrow(t_record(), t_type()))}, {:tuple, "PatVar", mk_scheme([], t_arrow(t_string(), t_pattern()))}, {:tuple, "PatWildcard", mk_scheme([], t_pattern())}, {:tuple, "PatLit", mk_scheme([], t_arrow(t_literal(), t_pattern()))}, {:tuple, "PatCon", mk_scheme([], t_arrow(t_string(), t_arrow(t_array(t_pattern()), t_pattern())))}, {:tuple, "PatRecord", mk_scheme([], t_arrow(t_array(t_tuple([t_string(), t_pattern()])), t_pattern()))}, {:tuple, "PatList", mk_scheme([], t_arrow(t_array(t_pattern()), t_pattern()))}, {:tuple, "PatCons", mk_scheme([], t_arrow(t_pattern(), t_arrow(t_pattern(), t_pattern())))}, {:tuple, "PatAs", mk_scheme([], t_arrow(t_string(), t_arrow(t_pattern(), t_pattern())))}, {:tuple, "PatParens", mk_scheme([], t_arrow(t_pattern(), t_pattern()))}, {:tuple, "PatTyped", mk_scheme([], t_arrow(t_pattern(), t_arrow(t_type_expr(), t_pattern())))}, {:tuple, "__guarded__", mk_scheme([a], {:ty_var, a})}, {:tuple, "_", mk_scheme([a], {:ty_var, a})}, {:tuple, "ExprVar", mk_scheme([], t_arrow(t_string(), t_expr()))}, {:tuple, "ExprLit", mk_scheme([], t_arrow(t_literal(), t_expr()))}, {:tuple, "ExprApp", mk_scheme([], t_arrow(t_expr(), t_arrow(t_expr(), t_expr())))}, {:tuple, "ExprLambda", mk_scheme([], t_arrow(t_array(t_pattern()), t_arrow(t_expr(), t_expr())))}, {:tuple, "ExprLet", mk_scheme([], t_arrow(t_array(t_let_bind()), t_arrow(t_expr(), t_expr())))}, {:tuple, "ExprIf", mk_scheme([], t_arrow(t_expr(), t_arrow(t_expr(), t_arrow(t_expr(), t_expr()))))}, {:tuple, "ExprCase", mk_scheme([], t_arrow(t_expr(), t_arrow(t_array(t_case_clause()), t_expr())))}, {:tuple, "ExprBinOp", mk_scheme([], t_arrow(t_string(), t_arrow(t_expr(), t_arrow(t_expr(), t_expr()))))}, {:tuple, "ExprList", mk_scheme([], t_arrow(t_array(t_expr()), t_expr()))}, {:tuple, "ExprRecord", mk_scheme([], t_arrow(t_array(t_tuple([t_string(), t_expr()])), t_expr()))}, {:tuple, "ExprRecordAccess", mk_scheme([], t_arrow(t_expr(), t_arrow(t_string(), t_expr())))}, {:tuple, "ExprParens", mk_scheme([], t_arrow(t_expr(), t_expr()))}, {:tuple, "ExprDo", mk_scheme([], t_arrow(t_array(t_do_statement()), t_expr()))}, {:tuple, "ExprQualified", mk_scheme([], t_arrow(t_string(), t_arrow(t_string(), t_expr())))}, {:tuple, "ExprRecordUpdate", mk_scheme([], t_arrow(t_expr(), t_arrow(t_array(t_tuple([t_string(), t_expr()])), t_expr())))}, {:tuple, "ExprTyped", mk_scheme([], t_arrow(t_expr(), t_arrow(t_type_expr(), t_expr())))}, {:tuple, "ExprUnaryOp", mk_scheme([], t_arrow(t_string(), t_arrow(t_expr(), t_expr())))}, {:tuple, "ExprTuple", mk_scheme([], t_arrow(t_array(t_expr()), t_expr()))}, {:tuple, "ExprSection", mk_scheme([], t_arrow(t_string(), t_expr()))}, {:tuple, "ExprSectionLeft", mk_scheme([], t_arrow(t_expr(), t_arrow(t_string(), t_expr())))}, {:tuple, "ExprSectionRight", mk_scheme([], t_arrow(t_string(), t_arrow(t_expr(), t_expr())))}, {:tuple, "ExprNegate", mk_scheme([], t_arrow(t_expr(), t_expr()))}, {:tuple, "LitInt", mk_scheme([], t_arrow(t_int(), t_literal()))}, {:tuple, "LitString", mk_scheme([], t_arrow(t_string(), t_literal()))}, {:tuple, "LitChar", mk_scheme([], t_arrow(t_char(), t_literal()))}, {:tuple, "LitBool", mk_scheme([], t_arrow(t_bool(), t_literal()))}, {:tuple, "LitNumber", mk_scheme([], t_arrow(t_number(), t_literal()))}, {:tuple, "TyExprCon", mk_scheme([], t_arrow(t_string(), t_type_expr()))}, {:tuple, "TyExprVar", mk_scheme([], t_arrow(t_string(), t_type_expr()))}, {:tuple, "TyExprApp", mk_scheme([], t_arrow(t_type_expr(), t_arrow(t_type_expr(), t_type_expr())))}, {:tuple, "TyExprArrow", mk_scheme([], t_arrow(t_type_expr(), t_arrow(t_type_expr(), t_type_expr())))}, {:tuple, "TyExprRecord", mk_scheme([], t_arrow(t_array(t_tuple([t_string(), t_type_expr()])), t_arrow(t_maybe(t_string()), t_type_expr())))}, {:tuple, "TyExprForAll", mk_scheme([], t_arrow(t_array(t_string()), t_arrow(t_type_expr(), t_type_expr())))}, {:tuple, "TyExprTuple", mk_scheme([], t_arrow(t_array(t_type_expr()), t_type_expr()))}, {:tuple, "TyExprConstrained", mk_scheme([], t_arrow(t_array(t_constraint()), t_arrow(t_type_expr(), t_type_expr())))}, {:tuple, "TyExprParens", mk_scheme([], t_arrow(t_type_expr(), t_type_expr()))}, {:tuple, "DeclFunction", mk_scheme([], t_arrow(t_function_decl(), t_declaration()))}, {:tuple, "DeclTypeSig", mk_scheme([], t_arrow(t_type_sig(), t_declaration()))}, {:tuple, "DeclDataType", mk_scheme([], t_arrow(t_data_type(), t_declaration()))}, {:tuple, "DeclTypeAlias", mk_scheme([], t_arrow(t_type_alias(), t_declaration()))}, {:tuple, "DeclModule", mk_scheme([], t_arrow(t_module_decl(), t_declaration()))}, {:tuple, "DeclImport", mk_scheme([], t_arrow(t_import_decl(), t_declaration()))}, {:tuple, "Ast.ExprVar", mk_scheme([], t_arrow(t_string(), t_expr()))}, {:tuple, "Ast.ExprLit", mk_scheme([], t_arrow(t_literal(), t_expr()))}, {:tuple, "Ast.ExprApp", mk_scheme([], t_arrow(t_expr(), t_arrow(t_expr(), t_expr())))}, {:tuple, "Ast.ExprLambda", mk_scheme([], t_arrow(t_array(t_pattern()), t_arrow(t_expr(), t_expr())))}, {:tuple, "Ast.ExprLet", mk_scheme([], t_arrow(t_array(t_let_bind()), t_arrow(t_expr(), t_expr())))}, {:tuple, "Ast.ExprIf", mk_scheme([], t_arrow(t_expr(), t_arrow(t_expr(), t_arrow(t_expr(), t_expr()))))}, {:tuple, "Ast.ExprCase", mk_scheme([], t_arrow(t_expr(), t_arrow(t_array(t_case_clause()), t_expr())))}, {:tuple, "Ast.ExprBinOp", mk_scheme([], t_arrow(t_string(), t_arrow(t_expr(), t_arrow(t_expr(), t_expr()))))}, {:tuple, "Ast.ExprList", mk_scheme([], t_arrow(t_array(t_expr()), t_expr()))}, {:tuple, "Ast.ExprRecord", mk_scheme([], t_arrow(t_array(t_tuple([t_string(), t_expr()])), t_expr()))}, {:tuple, "Ast.ExprRecordAccess", mk_scheme([], t_arrow(t_expr(), t_arrow(t_string(), t_expr())))}, {:tuple, "Ast.ExprParens", mk_scheme([], t_arrow(t_expr(), t_expr()))}, {:tuple, "Ast.ExprQualified", mk_scheme([], t_arrow(t_string(), t_arrow(t_string(), t_expr())))}, {:tuple, "Ast.ExprDo", mk_scheme([], t_arrow(t_array(t_do_statement()), t_expr()))}, {:tuple, "Ast.ExprRecordUpdate", mk_scheme([], t_arrow(t_expr(), t_arrow(t_array(t_tuple([t_string(), t_expr()])), t_expr())))}, {:tuple, "Ast.ExprTyped", mk_scheme([], t_arrow(t_expr(), t_arrow(t_type_expr(), t_expr())))}, {:tuple, "Ast.ExprSection", mk_scheme([], t_arrow(t_string(), t_expr()))}, {:tuple, "Ast.ExprSectionLeft", mk_scheme([], t_arrow(t_expr(), t_arrow(t_string(), t_expr())))}, {:tuple, "Ast.ExprSectionRight", mk_scheme([], t_arrow(t_string(), t_arrow(t_expr(), t_expr())))}, {:tuple, "Ast.ExprNegate", mk_scheme([], t_arrow(t_expr(), t_expr()))}, {:tuple, "Ast.ExprTuple", mk_scheme([], t_arrow(t_array(t_expr()), t_expr()))}, {:tuple, "Ast.ExprUnaryOp", mk_scheme([], t_arrow(t_string(), t_arrow(t_expr(), t_expr())))}, {:tuple, "Ast.PatVar", mk_scheme([], t_arrow(t_string(), t_pattern()))}, {:tuple, "Ast.PatWildcard", mk_scheme([], t_pattern())}, {:tuple, "Ast.PatLit", mk_scheme([], t_arrow(t_literal(), t_pattern()))}, {:tuple, "Ast.PatCon", mk_scheme([], t_arrow(t_string(), t_arrow(t_array(t_pattern()), t_pattern())))}, {:tuple, "Ast.PatRecord", mk_scheme([], t_arrow(t_array(t_tuple([t_string(), t_pattern()])), t_pattern()))}, {:tuple, "Ast.PatList", mk_scheme([], t_arrow(t_array(t_pattern()), t_pattern()))}, {:tuple, "Ast.PatCons", mk_scheme([], t_arrow(t_pattern(), t_arrow(t_pattern(), t_pattern())))}, {:tuple, "Ast.PatAs", mk_scheme([], t_arrow(t_string(), t_arrow(t_pattern(), t_pattern())))}, {:tuple, "Ast.PatParens", mk_scheme([], t_arrow(t_pattern(), t_pattern()))}, {:tuple, "Ast.PatTyped", mk_scheme([], t_arrow(t_pattern(), t_arrow(t_type_expr(), t_pattern())))}, {:tuple, "Ast.LitInt", mk_scheme([], t_arrow(t_int(), t_literal()))}, {:tuple, "Ast.LitString", mk_scheme([], t_arrow(t_string(), t_literal()))}, {:tuple, "Ast.LitChar", mk_scheme([], t_arrow(t_char(), t_literal()))}, {:tuple, "Ast.LitBool", mk_scheme([], t_arrow(t_bool(), t_literal()))}, {:tuple, "Ast.LitNumber", mk_scheme([], t_arrow(t_number(), t_literal()))}, {:tuple, "Ast.TyExprCon", mk_scheme([], t_arrow(t_string(), t_type_expr()))}, {:tuple, "Ast.TyExprVar", mk_scheme([], t_arrow(t_string(), t_type_expr()))}, {:tuple, "Ast.TyExprApp", mk_scheme([], t_arrow(t_type_expr(), t_arrow(t_type_expr(), t_type_expr())))}, {:tuple, "Ast.TyExprArrow", mk_scheme([], t_arrow(t_type_expr(), t_arrow(t_type_expr(), t_type_expr())))}, {:tuple, "Ast.TyExprRecord", mk_scheme([], t_arrow(t_array(t_tuple([t_string(), t_type_expr()])), t_arrow(t_maybe(t_string()), t_type_expr())))}, {:tuple, "Ast.TyExprForAll", mk_scheme([], t_arrow(t_array(t_string()), t_arrow(t_type_expr(), t_type_expr())))}, {:tuple, "Ast.TyExprConstrained", mk_scheme([], t_arrow(t_array(t_constraint()), t_arrow(t_type_expr(), t_type_expr())))}, {:tuple, "Ast.TyExprTuple", mk_scheme([], t_arrow(t_array(t_type_expr()), t_type_expr()))}, {:tuple, "Ast.DeclFunction", mk_scheme([], t_arrow(t_function_decl(), t_declaration()))}, {:tuple, "Ast.DeclTypeSig", mk_scheme([], t_arrow(t_type_sig(), t_declaration()))}, {:tuple, "Ast.DeclDataType", mk_scheme([], t_arrow(t_data_type(), t_declaration()))}, {:tuple, "Ast.DeclTypeAlias", mk_scheme([], t_arrow(t_type_alias(), t_declaration()))}, {:tuple, "Ast.DeclModule", mk_scheme([], t_arrow(t_module_decl(), t_declaration()))}, {:tuple, "Ast.DeclImport", mk_scheme([], t_arrow(t_import_decl(), t_declaration()))}, {:tuple, "Ast.DeclTypeClass", mk_scheme([], t_arrow(t_type_class(), t_declaration()))}, {:tuple, "Ast.DeclTypeClassInstance", mk_scheme([], t_arrow(t_type_class_instance(), t_declaration()))}, {:tuple, "Ast.DeclInfixDecl", mk_scheme([], t_arrow(t_infix_decl(), t_declaration()))}, {:tuple, "Ast.DeclForeignImport", mk_scheme([], t_arrow(t_foreign_import(), t_declaration()))}, {:tuple, "Ast.DeclType", mk_scheme([], t_arrow(t_type_decl(), t_declaration()))}, {:tuple, "Ast.DoLet", mk_scheme([], t_arrow(t_array(t_let_bind()), t_do_statement()))}, {:tuple, "Ast.DoBind", mk_scheme([], t_arrow(t_pattern(), t_arrow(t_expr(), t_do_statement())))}, {:tuple, "Ast.DoExpr", mk_scheme([], t_arrow(t_expr(), t_do_statement()))}, {:tuple, "DoLet", mk_scheme([], t_arrow(t_array(t_let_bind()), t_do_statement()))}, {:tuple, "DoBind", mk_scheme([], t_arrow(t_pattern(), t_arrow(t_expr(), t_do_statement())))}, {:tuple, "DoExpr", mk_scheme([], t_arrow(t_expr(), t_do_statement()))}, {:tuple, "Ast.GuardExpr", mk_scheme([], t_arrow(t_expr(), t_guard_clause()))}, {:tuple, "Ast.GuardPat", mk_scheme([], t_arrow(t_pattern(), t_arrow(t_expr(), t_guard_clause())))}, {:tuple, "GuardExpr", mk_scheme([], t_arrow(t_expr(), t_guard_clause()))}, {:tuple, "GuardPat", mk_scheme([], t_arrow(t_pattern(), t_arrow(t_expr(), t_guard_clause())))}, {:tuple, "Ast.LetBind", mk_scheme([], t_arrow(t_pattern(), t_arrow(t_expr(), t_let_bind())))}, {:tuple, "Ast.ImportValue", mk_scheme([], t_arrow(t_string(), t_import_item()))}, {:tuple, "Ast.ImportType", mk_scheme([], t_arrow(t_string(), t_arrow(t_import_spec(), t_import_item())))}, {:tuple, "Ast.ImportAll", mk_scheme([], t_import_spec())}, {:tuple, "Ast.ImportSome", mk_scheme([], t_arrow(t_array(t_string()), t_import_spec()))}, {:tuple, "Ast.ImportNone", mk_scheme([], t_import_spec())}])
  end

  def t_subst() do
    t_map(t_int(), t_type())
  end

  def t_env() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "bindings", t_map(t_string(), t_scheme())}, {:tuple, "counter", t_int()}, {:tuple, "registryLayer", t_maybe(t_int())}, {:tuple, "namespace", t_maybe(t_string())}]), row: :nothing}}
  end

  def t_scheme() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "vars", t_array(t_tvar())}, {:tuple, "ty", t_type()}]), row: :nothing}}
  end

  def t_type() do
    {:ty_con, mk_tcon0("Type")}
  end

  def t_tvar() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "id", t_int()}, {:tuple, "name", t_string()}]), row: :nothing}}
  end

  def t_tcon() do
    t_tcon_record()
  end

  def t_tcon_record() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", t_string()}, {:tuple, "args", t_array(t_type())}]), row: :nothing}}
  end

  def t_record() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "fields", t_map(t_string(), t_type())}, {:tuple, "row", t_maybe(t_tvar())}]), row: :nothing}}
  end

  def t_pattern() do
    {:ty_con, mk_tcon0("Pattern")}
  end

  def t_expr() do
    {:ty_con, mk_tcon0("Expr")}
  end

  def t_literal() do
    {:ty_con, mk_tcon0("Literal")}
  end

  def t_type_expr() do
    {:ty_con, mk_tcon0("TypeExpr")}
  end

  def t_let_bind() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "pattern", t_pattern()}, {:tuple, "value", t_expr()}, {:tuple, "typeAnn", t_maybe(t_type_expr())}]), row: :nothing}}
  end

  def t_case_clause() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "pattern", t_pattern()}, {:tuple, "guard", t_maybe(t_expr())}, {:tuple, "body", t_expr()}]), row: :nothing}}
  end

  def t_declaration() do
    {:ty_con, mk_tcon0("Declaration")}
  end

  def t_function_decl() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", t_string()}, {:tuple, "parameters", t_array(t_pattern())}, {:tuple, "body", t_expr()}, {:tuple, "guards", t_array(t_guarded_expr_rec())}, {:tuple, "typeSignature", t_maybe(t_type_sig_rec())}]), row: :nothing}}
  end

  def t_guarded_expr_rec() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "guards", t_array(t_guard_clause())}, {:tuple, "body", t_expr()}]), row: :nothing}}
  end

  def t_type_sig_rec() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", t_string()}, {:tuple, "typeVars", t_array(t_string())}, {:tuple, "constraints", t_array(t_constraint())}, {:tuple, "ty", t_type_expr()}]), row: :nothing}}
  end

  def t_type_sig() do
    t_type_sig_rec()
  end

  def t_data_type() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", t_string()}, {:tuple, "typeVars", t_array(t_string())}, {:tuple, "constructors", t_array(t_data_constructor())}]), row: :nothing}}
  end

  def t_data_constructor() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", t_string()}, {:tuple, "fields", t_array(t_data_field())}, {:tuple, "isRecord", t_bool()}]), row: :nothing}}
  end

  def t_data_field() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "label", t_string()}, {:tuple, "ty", t_type_expr()}]), row: :nothing}}
  end

  def t_type_alias() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", t_string()}, {:tuple, "typeVars", t_array(t_string())}, {:tuple, "ty", t_type_expr()}]), row: :nothing}}
  end

  def t_module_decl() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", t_string()}]), row: :nothing}}
  end

  def t_import_decl() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "moduleName", t_string()}, {:tuple, "alias", t_maybe(t_string())}, {:tuple, "items", t_array(t_import_item())}, {:tuple, "hiding", t_bool()}]), row: :nothing}}
  end

  def t_do_statement() do
    {:ty_con, mk_tcon0("DoStatement")}
  end

  def t_guarded_expr() do
    t_guarded_expr_rec()
  end

  def t_guard_clause() do
    {:ty_con, mk_tcon0("GuardClause")}
  end

  def t_type_class() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", t_string()}, {:tuple, "typeVars", t_array(t_string())}, {:tuple, "methods", t_array(t_type_sig())}, {:tuple, "kind", t_maybe(t_string())}]), row: :nothing}}
  end

  def t_type_class_instance() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "className", t_string()}, {:tuple, "ty", t_type_expr()}, {:tuple, "methods", t_array(t_function_decl())}, {:tuple, "derived", t_bool()}]), row: :nothing}}
  end

  def t_infix_decl() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "associativity", t_string()}, {:tuple, "precedence", t_int()}, {:tuple, "operator", t_string()}]), row: :nothing}}
  end

  def t_constraint() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "className", t_string()}, {:tuple, "types", t_array(t_type_expr())}]), row: :nothing}}
  end

  def t_import_item() do
    {:ty_con, mk_tcon0("ImportItem")}
  end

  def t_import_spec() do
    {:ty_con, mk_tcon0("ImportSpec")}
  end

  def t_foreign_import() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "moduleName", t_string()}, {:tuple, "functionName", t_string()}, {:tuple, "ty", t_type_expr()}]), row: :nothing}}
  end

  def t_type_decl() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "name", t_string()}, {:tuple, "ty", t_type_expr()}]), row: :nothing}}
  end

  def t_unify_error() do
    {:ty_con, mk_tcon0("UnifyError")}
  end

  def t_tcerror() do
    {:ty_con, mk_tcon0("TCError")}
  end

  def t_pat_result() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "env", t_env()}, {:tuple, "sub", t_subst()}]), row: :nothing}}
  end

  def t_instantiate_result() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "ty", t_type()}, {:tuple, "env", t_env()}]), row: :nothing}}
  end

  def t_infer_result() do
    {:ty_record, %{fields: Nova.Map.from_foldable([{:tuple, "ty", t_type()}, {:tuple, "env", t_env()}, {:tuple, "sub", t_subst()}]), row: :nothing}}
  end

  # @type type_alias_info :: %{params: array()(string()), body: type_expr()}

  # @type module_exports :: %{types: map()(string())(type_info()), constructors: map()(string())(scheme()), values: map()(string())(scheme()), type_aliases: map()(string())(type_alias_info())}

  # @type type_info :: %{arity: int(), constructors: array()(string())}

  def empty_exports() do
    %{types: Nova.Map.empty, constructors: Nova.Map.empty, values: Nova.Map.empty, type_aliases: Nova.Map.empty}
  end

  # @type module_registry :: map()(string())(module_exports())

  def empty_registry() do
    Nova.Map.empty
  end

  def lookup_module(reg, name) do
    Nova.Map.lookup(name, reg)
  end

  def register_module(reg, name, exports) do
    Nova.Map.insert(name, exports, reg)
  end

  def merge_exports_to_env(env, exports) do
    
      ctor_list = Nova.Map.to_unfoldable(exports.constructors)
      val_list = Nova.Map.to_unfoldable(exports.values)
      env1 = Nova.Array.foldl(fn e -> fn ({:tuple, name, scheme}) -> extend_env(e, name, scheme) end end, env, ctor_list)
      env2 = Nova.Array.foldl(fn e -> fn ({:tuple, name, scheme}) -> extend_env(e, name, scheme) end end, env1, val_list)
      env2
  end

  def merge_selected_exports(env, exports, items) do
    
      add_item = fn e -> fn name -> case Nova.Map.lookup(name, exports.constructors) do
        {:just, scheme} -> extend_env(e, name, scheme)
        :nothing -> case Nova.Map.lookup(name, exports.values) do
            {:just, scheme} -> extend_env(e, name, scheme)
            :nothing -> e
          end
      end end end
      Nova.Array.foldl(add_item, env, items)
  end

  def merge_type_export(env, exports, type_name, ctor_names) do
    
      add_ctor = fn e -> fn ctor_name -> case Nova.Map.lookup(ctor_name, exports.constructors) do
        {:just, scheme} -> extend_env(e, ctor_name, scheme)
        :nothing -> e
      end end end
      Nova.Array.foldl(add_ctor, env, ctor_names)
  end
end
