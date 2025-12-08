# FFI module for FFIConstraintWorkaround test
# Corresponds to FFIConstraintWorkaround.js:
# export function showImpl(showFn) {
#   return function (val) {
#     return showFn(val);
#   };
# };

defmodule Nova.FFI.FFIConstraintWorkaround do
  # showImpl takes a show function and returns a function that applies it
  # In curried style: showImpl(showFn) -> (val -> showFn(val))
  def show_impl(show_fn) do
    fn val -> show_fn.(val) end
  end
end
