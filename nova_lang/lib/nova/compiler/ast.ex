defmodule Nova.Compiler.Ast do
  # import Prelude

  # import Data.Maybe

  # import Data.Tuple

  # @type module :: %{name: string(), declarations: array()(declaration())}

  # Data type: Declaration
  def decl_module(arg0), do: {:decl_module, arg0}
  def decl_function(arg0), do: {:decl_function, arg0}
  def decl_type(arg0), do: {:decl_type, arg0}
  def decl_type_alias(arg0), do: {:decl_type_alias, arg0}
  def decl_data_type(arg0), do: {:decl_data_type, arg0}
  def decl_type_class(arg0), do: {:decl_type_class, arg0}
  def decl_type_class_instance(arg0), do: {:decl_type_class_instance, arg0}
  def decl_import(arg0), do: {:decl_import, arg0}
  def decl_foreign_import(arg0), do: {:decl_foreign_import, arg0}
  def decl_type_sig(arg0), do: {:decl_type_sig, arg0}

  # @type function_declaration :: %{name: string(), parameters: array()(pattern()), body: expr(), guards: array()(guarded_expr()), type_signature: maybe()(type_signature())}

  # @type guarded_expr :: %{guards: array()(guard_clause()), body: expr()}

  # Data type: GuardClause
  def guard_expr(arg0), do: {:guard_expr, arg0}
  def guard_pat(arg0, arg1), do: {:guard_pat, arg0, arg1}

  # @type type_declaration :: %{name: string(), type_signature: type_expr()}

  # @type type_signature :: %{name: string(), type_vars: array()(string()), constraints: array()(constraint()), ty: type_expr()}

  # @type type_class :: %{name: string(), type_vars: array()(string()), methods: array()(type_signature()), kind: maybe()(string())}

  # @type type_alias :: %{name: string(), type_vars: array()(string()), ty: type_expr()}

  # @type type_class_instance :: %{class_name: string(), ty: type_expr(), methods: array()(function_declaration()), derived: boolean()}

  # @type data_type :: %{name: string(), type_vars: array()(string()), constructors: array()(data_constructor())}

  # @type data_constructor :: %{name: string(), fields: array()(data_field()), is_record: boolean()}

  # @type data_field :: %{label: string(), ty: type_expr()}

  # @type import_declaration :: %{module_name: string(), alias_: maybe()(string()), items: array()(import_item()), hiding: boolean()}

  # Data type: ImportItem
  def import_value(arg0), do: {:import_value, arg0}
  def import_type(arg0, arg1), do: {:import_type, arg0, arg1}

  # Data type: ImportSpec
  def import_all(), do: :import_all
  def import_some(arg0), do: {:import_some, arg0}
  def import_none(), do: :import_none

  # @type foreign_import :: %{module_name: string(), function_name: string(), alias_: maybe()(string()), type_signature: type_expr()}

  # @type constraint :: %{class_name: string(), types: array()(type_expr())}

  # Data type: TypeExpr
  def ty_expr_con(arg0), do: {:ty_expr_con, arg0}
  def ty_expr_var(arg0), do: {:ty_expr_var, arg0}
  def ty_expr_app(arg0, arg1), do: {:ty_expr_app, arg0, arg1}
  def ty_expr_arrow(arg0, arg1), do: {:ty_expr_arrow, arg0, arg1}
  def ty_expr_record(arg0, arg1), do: {:ty_expr_record, arg0, arg1}
  def ty_expr_for_all(arg0, arg1), do: {:ty_expr_for_all, arg0, arg1}
  def ty_expr_constrained(arg0, arg1), do: {:ty_expr_constrained, arg0, arg1}
  def ty_expr_parens(arg0), do: {:ty_expr_parens, arg0}
  def ty_expr_tuple(arg0), do: {:ty_expr_tuple, arg0}

  # Data type: Pattern
  def pat_var(arg0), do: {:pat_var, arg0}
  def pat_wildcard(), do: :pat_wildcard
  def pat_lit(arg0), do: {:pat_lit, arg0}
  def pat_con(arg0, arg1), do: {:pat_con, arg0, arg1}
  def pat_record(arg0), do: {:pat_record, arg0}
  def pat_list(arg0), do: {:pat_list, arg0}
  def pat_cons(arg0, arg1), do: {:pat_cons, arg0, arg1}
  def pat_as(arg0, arg1), do: {:pat_as, arg0, arg1}
  def pat_parens(arg0), do: {:pat_parens, arg0}

  # Data type: Expr
  def expr_var(arg0), do: {:expr_var, arg0}
  def expr_qualified(arg0, arg1), do: {:expr_qualified, arg0, arg1}
  def expr_lit(arg0), do: {:expr_lit, arg0}
  def expr_app(arg0, arg1), do: {:expr_app, arg0, arg1}
  def expr_lambda(arg0, arg1), do: {:expr_lambda, arg0, arg1}
  def expr_let(arg0, arg1), do: {:expr_let, arg0, arg1}
  def expr_if(arg0, arg1, arg2), do: {:expr_if, arg0, arg1, arg2}
  def expr_case(arg0, arg1), do: {:expr_case, arg0, arg1}
  def expr_do(arg0), do: {:expr_do, arg0}
  def expr_bin_op(arg0, arg1, arg2), do: {:expr_bin_op, arg0, arg1, arg2}
  def expr_unary_op(arg0, arg1), do: {:expr_unary_op, arg0, arg1}
  def expr_list(arg0), do: {:expr_list, arg0}
  def expr_tuple(arg0), do: {:expr_tuple, arg0}
  def expr_record(arg0), do: {:expr_record, arg0}
  def expr_record_access(arg0, arg1), do: {:expr_record_access, arg0, arg1}
  def expr_record_update(arg0, arg1), do: {:expr_record_update, arg0, arg1}
  def expr_typed(arg0, arg1), do: {:expr_typed, arg0, arg1}
  def expr_parens(arg0), do: {:expr_parens, arg0}
  def expr_section(arg0), do: {:expr_section, arg0}

  # Data type: Literal
  def lit_int(arg0), do: {:lit_int, arg0}
  def lit_number(arg0), do: {:lit_number, arg0}
  def lit_string(arg0), do: {:lit_string, arg0}
  def lit_char(arg0), do: {:lit_char, arg0}
  def lit_bool(arg0), do: {:lit_bool, arg0}

  # @type let_bind :: %{pattern: pattern(), value: expr(), type_ann: maybe()(type_expr())}

  # @type case_clause :: %{pattern: pattern(), guard: maybe()(expr()), body: expr()}

  # Data type: DoStatement
  def do_let(arg0), do: {:do_let, arg0}
  def do_bind(arg0, arg1), do: {:do_bind, arg0, arg1}
  def do_expr(arg0), do: {:do_expr, arg0}
end
