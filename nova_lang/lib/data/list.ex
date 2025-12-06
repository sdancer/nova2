defmodule Data.List do
  @moduledoc "Data.List - list manipulation functions"

  defdelegate length(xs), to: Nova.Array
  defdelegate head(xs), to: Nova.Array
  defdelegate tail(xs), to: Nova.Array
  defdelegate reverse(xs), to: Nova.Array
  defdelegate concat(xs), to: Nova.Array
  defdelegate concat_map(f, xs), to: Nova.Array
  defdelegate map(f, xs), to: Nova.Array
  defdelegate filter(f, xs), to: Nova.Array
  defdelegate take(n, xs), to: Nova.Array
  defdelegate drop(n, xs), to: Nova.Array
  defdelegate elem(x, xs), to: Nova.Array
  defdelegate find(f, xs), to: Nova.Array
  defdelegate find_index(f, xs), to: Nova.Array
  defdelegate any(f, xs), to: Nova.Array
  defdelegate all(f, xs), to: Nova.Array
  defdelegate zip(xs, ys), to: Nova.Array
  defdelegate zip_with(f, xs, ys), to: Nova.Array
  defdelegate unzip(xs), to: Nova.Array
  defdelegate sort(xs), to: Nova.Array
  defdelegate sort_by(f, xs), to: Nova.Array
  defdelegate nub(xs), to: Nova.Array
  defdelegate intercalate(sep, xs), to: Nova.Array
  defdelegate replicate(n, x), to: Nova.Array
  defdelegate null(xs), to: Nova.Array
  defdelegate cons(x, xs), to: Nova.Array
  defdelegate snoc(xs, x), to: Nova.Array
  defdelegate foldl(f, acc, xs), to: Nova.Array
  defdelegate foldr(f, acc, xs), to: Nova.Array
end
