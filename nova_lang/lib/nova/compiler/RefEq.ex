defmodule Nova.Compiler.RefEq do
  # import Nova.Compiler.Ast

  def ref_eq_expr(({:expr_var, a}), ({:expr_var, b})) do
    (a == b)
  end

  def ref_eq_expr(({:expr_lit, a}), ({:expr_lit, b})) do
    true
  end

  def ref_eq_expr(({:expr_app, f1, a1}), ({:expr_app, f2, a2})) do
    (ref_eq_expr(f1, f2) and ref_eq_expr(a1, a2))
  end

  def ref_eq_expr(({:expr_lambda, p1, b1}), ({:expr_lambda, p2, b2})) do
    ((Nova.Runtime.length(p1) == Nova.Runtime.length(p2)) and ref_eq_expr(b1, b2))
  end

  def ref_eq_expr(({:expr_parens, e1}), ({:expr_parens, e2})) do
    ref_eq_expr(e1, e2)
  end

  def ref_eq_expr(({:expr_parens, e1}), e2) do
    ref_eq_expr(e1, e2)
  end

  def ref_eq_expr(e1, ({:expr_parens, e2})) do
    ref_eq_expr(e1, e2)
  end

  def ref_eq_expr(_, _) do
    false
  end
end
