# FFI module for RowUnion test
# Corresponds to RowUnion.js:
# export var mergeImpl = function (l) {
#   return function (r) {
#     var o = {};
#     return Object.assign(o, r, l);
#   };
# };

defmodule Nova.FFI.RowUnion do
  # mergeImpl: Record r1 -> Record r2 -> Record r3
  # Merges two records, with left taking precedence
  # Curried: mergeImpl(l) -> (r -> Map.merge(r, l))
  def merge_impl(l) do
    fn r -> Map.merge(r, l) end
  end
end
