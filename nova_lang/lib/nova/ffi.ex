# Default FFI module for Nova
# Contains common FFI functions for tests and general usage
# Module-specific FFI can be placed in Nova.FFI.<ModuleName>

defmodule Nova.FFI do
  # Identity function (from ESFFIFunctionConst, ESFFIFunctionFunction, ESFFIFunctionVar tests)
  def function_name(a), do: a

  # Value constant (from ESFFIValueConst1, ESFFIValueVar tests)
  def value(), do: 1

  # Default export (from FFIDefaultESExport test)
  def default(), do: "Done"

  # showImpl - wraps a show function (from FFIConstraintWorkaround test)
  # showImpl(showFn) -> (val -> showFn(val))
  def show_impl(show_fn) do
    fn val -> show_fn.(val) end
  end

  # add3 - concatenates 3 strings (from EffFn test)
  # This is an EffectFn3, so it takes 3 args at once
  def add3(a, b, c), do: a <> b <> c

  # PolyLabels FFI
  # unsafeGet: String -> Record r -> a
  def unsafe_get(s) do
    fn o -> Map.get(o, String.to_atom(s)) end
  end

  # unsafeSet: String -> a -> Record r1 -> Record r2
  def unsafe_set(s) do
    fn a ->
      fn o -> Map.put(o, String.to_atom(s), a) end
    end
  end

  # RowUnion FFI
  # mergeImpl: Record r1 -> Record r2 -> Record r3
  def merge_impl(l) do
    fn r -> Map.merge(r, l) end
  end

  # FunWithFunDeps FFI
  def fnil(), do: []

  def fcons(hd) do
    fn tl -> [hd | tl] end
  end

  def fappend_impl(left) do
    fn right -> left ++ right end
  end

  def fflatten_impl(v), do: List.flatten(v)

  def fto_array(vect), do: vect
end
