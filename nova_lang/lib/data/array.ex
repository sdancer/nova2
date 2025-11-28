defmodule Data.Array do
  @moduledoc "Data.Array wrapper - delegates to Nova.Array"

  defdelegate head(xs), to: Nova.Array
  defdelegate last(xs), to: Nova.Array
  defdelegate tail(xs), to: Nova.Array
  defdelegate init(xs), to: Nova.Array
  defdelegate uncons(xs), to: Nova.Array
  defdelegate length(xs), to: Nova.Array
  defdelegate null(xs), to: Nova.Array
  defdelegate reverse(xs), to: Nova.Array
  defdelegate concat(xs), to: Nova.Array

  def elem(x, xs), do: Nova.Array.elem(x, xs)
  def cons(x, xs), do: Nova.Array.cons(x, xs)
  def snoc(xs, x), do: Nova.Array.snoc(xs, x)
  def take(n, xs), do: Nova.Array.take(n, xs)
  def drop(n, xs), do: Nova.Array.drop(n, xs)
  def filter(f, xs), do: Nova.Array.filter(f, xs)
  def find(f, xs), do: Nova.Array.find(f, xs)
  def index(x, xs), do: Nova.Array.index(x, xs)
  def map(f, xs), do: Nova.Array.map(f, xs)
  def map_with_index(f, xs), do: Nova.Array.map_with_index(f, xs)
  def foldl(f, acc, xs), do: Nova.Array.foldl(f, acc, xs)
  def foldr(f, acc, xs), do: Nova.Array.foldr(f, acc, xs)
  def zip(xs, ys), do: Nova.Array.zip(xs, ys)
  def zip_with(f, xs, ys), do: Nova.Array.zip_with(f, xs, ys)
  def concat_map(f, xs), do: Nova.Array.concat_map(f, xs)
  def range(a, b), do: Nova.Array.range(a, b)
  def replicate(n, x), do: Nova.Array.replicate(n, x)
  def sort_by(f, xs), do: Nova.Array.sort_by(f, xs)
  def all(f, xs), do: Nova.Array.all(f, xs)
  def any(f, xs), do: Nova.Array.any(f, xs)
  def drop_while(f, xs), do: Nova.Array.drop_while(f, xs)
  def span(f, xs), do: Nova.Array.span(f, xs)
  def partition(f, xs), do: Nova.Array.partition(f, xs)
end
