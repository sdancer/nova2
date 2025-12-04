defmodule Nova.Compiler.CodeGenWasmSimple do
  # import Prelude

  # import Data.Array

  # import Data.Array

  # import Data.Maybe

  # import Data.Tuple

  # import Data.String

  # import Data.Set

  # import Data.Set

  # import Data.Map

  # import Data.Map

  # import Data.Foldable

  # import Data.List

  # import Data.List

  # import Data.Int

  # import Data.Enum

  # import Nova.Compiler.Ast

  # @type lifted_lambda :: %{id: int(), expr: expr(), params: array()(string()), free_vars: array()(string()), body: expr()}

  # @type wasm_ctx :: %{module_name: string(), module_funcs: set()(string()), func_arities: map()(string())(int()), locals: map()(string())(int()), local_count: int(), string_literals: array()(string()), string_table: map()(string())(%{offset: int(), len: int()}), data_constructors: map()(string())(%{tag: int(), arity: int()}), func_table: array()(string()), lambdas: array()(lifted_lambda()), lambda_counter: int(), func_wrapper_idx: map()(string())(int())}



  def empty_ctx(mod_name) do
    %{module_name: mod_name, module_funcs: Nova.Set.empty, func_arities: Nova.Map.empty, locals: Nova.Map.empty, local_count: 0, string_literals: [], string_table: Nova.Map.empty, data_constructors: prelude_constructors(), func_table: [], lambdas: [], lambda_counter: 0, func_wrapper_idx: Nova.Map.empty}
  end



  def prelude_constructors() do
    Nova.Map.from_foldable([{:tuple, "Nothing", %{tag: 0, arity: 0}}, {:tuple, "Just", %{tag: 1, arity: 1}}, {:tuple, "Left", %{tag: 0, arity: 1}}, {:tuple, "Right", %{tag: 1, arity: 1}}, {:tuple, "Tuple", %{tag: 0, arity: 2}}, {:tuple, "Nil", %{tag: 0, arity: 0}}, {:tuple, "Cons", %{tag: 1, arity: 2}}, {:tuple, "TokKeyword", %{tag: 0, arity: 0}}, {:tuple, "TokIdentifier", %{tag: 1, arity: 0}}, {:tuple, "TokNumber", %{tag: 2, arity: 0}}, {:tuple, "TokString", %{tag: 3, arity: 0}}, {:tuple, "TokChar", %{tag: 4, arity: 0}}, {:tuple, "TokOperator", %{tag: 5, arity: 0}}, {:tuple, "TokDelimiter", %{tag: 6, arity: 0}}, {:tuple, "TokNewline", %{tag: 7, arity: 0}}, {:tuple, "TokUnrecognized", %{tag: 8, arity: 0}}])
  end



  def mangle_name(name) do
    
      escaped = Nova.String.replace_all((Nova.String.pattern("'")), (Nova.String.replacement("_prime_")), name)
      escaped2 = Nova.String.replace_all((Nova.String.pattern(".")), (Nova.String.replacement("_")), escaped)
      Nova.Runtime.append("$", escaped2)
  end



  def collect_do_stmt_refs(({:do_expr, e})) do
    collect_external_refs(e)
  end

  def collect_do_stmt_refs(({:do_bind, _, e})) do
    collect_external_refs(e)
  end

  def collect_do_stmt_refs(({:do_let, binds})) do
    list_concat_map.((fn b -> collect_external_refs(b.value) end)).(binds)
  end



  def collect_external_refs(({:expr_qualified, mod_name, name})) do
    [{:tuple, mod_name, name}]
  end

  def collect_external_refs(({:expr_app, f, a})) do
    Nova.Runtime.append(collect_external_refs(f), collect_external_refs(a))
  end

  def collect_external_refs(({:expr_lambda, _, body})) do
    collect_external_refs(body)
  end

  def collect_external_refs(({:expr_let, binds, body})) do
    Nova.Runtime.append(list_concat_map.((fn b -> collect_external_refs(b.value) end)).(binds), collect_external_refs(body))
  end

  def collect_external_refs(({:expr_if, c, t, e})) do
    Nova.Runtime.append(Nova.Runtime.append(collect_external_refs(c), collect_external_refs(t)), collect_external_refs(e))
  end

  def collect_external_refs(({:expr_case, s, clauses})) do
    Nova.Runtime.append(collect_external_refs(s), list_concat_map.((fn c -> collect_external_refs(c.body) end)).(clauses))
  end

  def collect_external_refs(({:expr_bin_op, _, l, r})) do
    Nova.Runtime.append(collect_external_refs(l), collect_external_refs(r))
  end

  def collect_external_refs(({:expr_unary_op, _, e})) do
    collect_external_refs(e)
  end

  def collect_external_refs(({:expr_list, es})) do
    list_concat_map.((&collect_external_refs/1)).(es)
  end

  def collect_external_refs(({:expr_tuple, es})) do
    list_concat_map.((&collect_external_refs/1)).(es)
  end

  def collect_external_refs(({:expr_record, fs})) do
    list_concat_map.((fn ({:tuple, _, e}) -> collect_external_refs(e) end)).(fs)
  end

  def collect_external_refs(({:expr_record_access, e, _})) do
    collect_external_refs(e)
  end

  def collect_external_refs(({:expr_record_update, e, fs})) do
    Nova.Runtime.append(collect_external_refs(e), list_concat_map.((fn ({:tuple, _, ex}) -> collect_external_refs(ex) end)).(fs))
  end

  def collect_external_refs(({:expr_parens, e})) do
    collect_external_refs(e)
  end

  def collect_external_refs(({:expr_do, stmts})) do
    list_concat_map_do.((&collect_do_stmt_refs/1)).(stmts)
  end

  def collect_external_refs(({:expr_typed, e, _})) do
    collect_external_refs(e)
  end

  def collect_external_refs(({:expr_section_left, e, _})) do
    collect_external_refs(e)
  end

  def collect_external_refs(({:expr_section_right, _, e})) do
    collect_external_refs(e)
  end

  def collect_external_refs(({:expr_var, name})) do
    case Nova.String.index_of((Nova.String.pattern(".")), name) do
      {:just, idx} -> [{:tuple, (Nova.String.take(idx, name)), (Nova.String.drop(((idx + 1)), name))}]
      :nothing -> []
    end
  end

  def collect_external_refs(_) do
    []
  end



  def collect_decl_refs(({:decl_function, f})) do
    collect_external_refs(f.body)
  end

  def collect_decl_refs(_) do
    []
  end



  def collect_strings(({:expr_lit, ({:lit_string, s})})) do
    [s]
  end

  def collect_strings(({:expr_app, f, a})) do
    Nova.Runtime.append(collect_strings(f), collect_strings(a))
  end

  def collect_strings(({:expr_lambda, _, body})) do
    collect_strings(body)
  end

  def collect_strings(({:expr_let, binds, body})) do
    Nova.Runtime.append(list_concat_map.((fn b -> collect_strings(b.value) end)).(binds), collect_strings(body))
  end

  def collect_strings(({:expr_if, c, t, e})) do
    Nova.Runtime.append(Nova.Runtime.append(collect_strings(c), collect_strings(t)), collect_strings(e))
  end

  def collect_strings(({:expr_case, s, clauses})) do
    Nova.Runtime.append(collect_strings(s), list_concat_map.((fn c -> collect_strings(c.body) end)).(clauses))
  end

  def collect_strings(({:expr_bin_op, _, l, r})) do
    Nova.Runtime.append(collect_strings(l), collect_strings(r))
  end

  def collect_strings(({:expr_unary_op, _, e})) do
    collect_strings(e)
  end

  def collect_strings(({:expr_list, es})) do
    list_concat_map.((&collect_strings/1)).(es)
  end

  def collect_strings(({:expr_tuple, es})) do
    list_concat_map.((&collect_strings/1)).(es)
  end

  def collect_strings(({:expr_record, fs})) do
    list_concat_map.((fn ({:tuple, _, e}) -> collect_strings(e) end)).(fs)
  end

  def collect_strings(({:expr_record_access, e, _})) do
    collect_strings(e)
  end

  def collect_strings(({:expr_record_update, e, fs})) do
    Nova.Runtime.append(collect_strings(e), list_concat_map.((fn ({:tuple, _, ex}) -> collect_strings(ex) end)).(fs))
  end

  def collect_strings(({:expr_parens, e})) do
    collect_strings(e)
  end

  def collect_strings(({:expr_do, stmts})) do
    list_concat_map_do.(collect_do_stmt).(stmts)
  end

  def collect_strings(({:expr_typed, e, _})) do
    collect_strings(e)
  end

  def collect_strings(({:expr_section_left, e, _})) do
    collect_strings(e)
  end

  def collect_strings(({:expr_section_right, _, e})) do
    collect_strings(e)
  end

  def collect_strings(_) do
    []
  end
end
