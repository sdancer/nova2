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
end
