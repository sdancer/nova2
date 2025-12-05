defmodule Nova.List do
  @moduledoc """
  Nova List module - provides linked list operations compatible with PureScript's Data.List.
  Now uses native Elixir lists [] and [h | t] for better interoperability.
  """
  import Kernel, except: [elem: 2]

  # Convert to Elixir list - returns the list as-is since we now use native lists
  def from_foldable(list) when is_list(list), do: list
  def from_foldable(other), do: Enum.to_list(other)

  # Convert a linked list to an Elixir list - identity for native lists
  # Also handles legacy {:cons, ...} format for backwards compatibility
  def to_list(:nil_), do: []
  def to_list({:cons, head, tail}), do: [head | to_list(tail)]
  def to_list(list) when is_list(list), do: list

  # Check if a list is empty
  def null([]), do: true
  def null([_ | _]), do: false
  # Legacy support
  def null(:nil_), do: true
  def null({:cons, _, _}), do: false

  # Get head and tail
  def uncons([]), do: :nothing
  def uncons([head | tail]), do: {:just, %{head: head, tail: tail}}
  # Legacy support
  def uncons(:nil_), do: :nothing
  def uncons({:cons, head, tail}), do: {:just, %{head: head, tail: to_list(tail)}}

  # Reverse a list
  def reverse(list), do: Enum.reverse(to_list(list))

  # Get the length of a list
  def length(list), do: Kernel.length(to_list(list))

  # Head of a list (returns Maybe)
  def head([]), do: :nothing
  def head([h | _]), do: {:just, h}
  # Legacy support
  def head(:nil_), do: :nothing
  def head({:cons, h, _}), do: {:just, h}

  # Tail of a list (returns Maybe)
  def tail([]), do: :nothing
  def tail([_ | t]), do: {:just, t}
  # Legacy support
  def tail(:nil_), do: :nothing
  def tail({:cons, _, t}), do: {:just, to_list(t)}

  # Cons (prepend) - now returns native list
  def cons(elem, list), do: [elem | to_list(list)]

  # Singleton list
  def singleton(elem), do: [elem]

  # Append two lists
  def append(xs, ys), do: to_list(xs) ++ to_list(ys)

  # Map over a list
  def map(f, list), do: Enum.map(to_list(list), f)

  # Filter a list
  def filter(p, list), do: Enum.filter(to_list(list), p)

  # Fold right
  def foldr(f, acc, list), do: List.foldr(to_list(list), acc, fn x, a -> f.(x, a) end)

  # Fold left
  def foldl(f, acc, list), do: List.foldl(to_list(list), acc, fn x, a -> f.(a, x) end)

  # Take first n elements
  def take(n, list), do: Enum.take(to_list(list), n)

  # Drop first n elements
  def drop(n, list), do: Enum.drop(to_list(list), n)

  # Any - check if any element satisfies predicate
  def any(p, list), do: Enum.any?(to_list(list), p)

  # Map Maybe - map with a function that returns Maybe, keeping only Just values
  def map_maybe(f, list) do
    Enum.flat_map(to_list(list), fn x ->
      case f.(x) do
        {:just, v} -> [v]
        :nothing -> []
      end
    end)
  end

  # Take while - take elements while predicate is true
  def take_while(p, list), do: Enum.take_while(to_list(list), p)

  # Range - create a list from start to end (inclusive)
  def range(start, finish) when start > finish, do: []
  def range(start, finish), do: Enum.to_list(start..finish)

  # To unfoldable - convert linked list to Elixir list (same as to_list)
  def to_unfoldable(list), do: to_list(list)

  # Check if element is in the list
  def elem(x, list), do: Enum.member?(to_list(list), x)
end
