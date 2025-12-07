defmodule Data.Foldable do
  @moduledoc "Data.Foldable - foldable functions for collections"

  # Use Nova.Runtime.foldl which handles maps, lists, and linked lists
  defdelegate foldl(f, acc, xs), to: Nova.Runtime
  defdelegate foldr(f, acc, xs), to: Nova.Array
  defdelegate fold_m(f, acc, xs), to: Nova.Runtime
  defdelegate foldl_m(f, acc, xs), to: Nova.Runtime, as: :fold_m
  defdelegate foldr_m(f, acc, xs), to: Nova.Runtime, as: :fold_m
  defdelegate any(f, xs), to: Nova.Array
  defdelegate all(f, xs), to: Nova.Array
  defdelegate elem(x, xs), to: Nova.Array
  defdelegate find(f, xs), to: Nova.Array
  defdelegate length(xs), to: Nova.Array
  defdelegate null(xs), to: Nova.Array
  defdelegate concat(xs), to: Nova.Array
end
