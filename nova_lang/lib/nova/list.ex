defmodule Nova.List do
  @moduledoc """
  Nova List module - provides linked list operations compatible with PureScript's Data.List.
  Nova uses PureScript-style linked lists (Cons/Nil) internally.
  """
  import Kernel, except: [elem: 2]

  # Convert an array/list to a linked list (Cons/Nil structure)
  def from_foldable(list) when is_list(list) do
    Enum.reduce(Enum.reverse(list), :nil, fn elem, acc ->
      {:cons, elem, acc}
    end)
  end

  def from_foldable(other), do: from_foldable(Enum.to_list(other))

  # Convert a linked list to an Elixir list
  def to_list(:nil), do: []
  def to_list({:cons, head, tail}), do: [head | to_list(tail)]
  def to_list(list) when is_list(list), do: list

  # Check if a list is empty
  def null(:nil), do: true
  def null({:cons, _, _}), do: false
  def null([]), do: true
  def null([_ | _]), do: false

  # Get head and tail
  def uncons(:nil), do: :nothing
  def uncons({:cons, head, tail}), do: {:just, {head, tail}}
  def uncons([]), do: :nothing
  def uncons([head | tail]), do: {:just, {head, from_foldable(tail)}}

  # Reverse a linked list
  def reverse(list) do
    reverse_acc(list, :nil)
  end

  defp reverse_acc(:nil, acc), do: acc
  defp reverse_acc({:cons, head, tail}, acc), do: reverse_acc(tail, {:cons, head, acc})
  defp reverse_acc([], acc), do: acc
  defp reverse_acc([head | tail], acc), do: reverse_acc(tail, {:cons, head, acc})

  # Get the length of a list
  def length(list), do: length_acc(list, 0)

  defp length_acc(:nil, acc), do: acc
  defp length_acc({:cons, _, tail}, acc), do: length_acc(tail, acc + 1)
  defp length_acc([], acc), do: acc
  defp length_acc([_ | tail], acc), do: length_acc(tail, acc + 1)

  # Head of a list (returns Maybe)
  def head(:nil), do: :nothing
  def head({:cons, h, _}), do: {:just, h}
  def head([]), do: :nothing
  def head([h | _]), do: {:just, h}

  # Tail of a list (returns Maybe)
  def tail(:nil), do: :nothing
  def tail({:cons, _, t}), do: {:just, t}
  def tail([]), do: :nothing
  def tail([_ | t]), do: {:just, from_foldable(t)}

  # Cons (prepend)
  def cons(elem, list), do: {:cons, elem, list}

  # Singleton list
  def singleton(elem), do: {:cons, elem, :nil}

  # Append two lists
  def append(:nil, ys), do: ys
  def append({:cons, x, xs}, ys), do: {:cons, x, append(xs, ys)}
  def append(xs, ys) when is_list(xs), do: append(from_foldable(xs), ys)

  # Map over a list
  def map(_f, :nil), do: :nil
  def map(f, {:cons, x, xs}), do: {:cons, f.(x), map(f, xs)}

  # Filter a list
  def filter(_p, :nil), do: :nil
  def filter(p, {:cons, x, xs}) do
    if p.(x), do: {:cons, x, filter(p, xs)}, else: filter(p, xs)
  end

  # Fold right
  def foldr(_f, acc, :nil), do: acc
  def foldr(f, acc, {:cons, x, xs}), do: f.(x, foldr(f, acc, xs))

  # Fold left
  def foldl(_f, acc, :nil), do: acc
  def foldl(f, acc, {:cons, x, xs}), do: foldl(f, f.(acc, x), xs)

  # Take first n elements
  def take(0, _), do: :nil
  def take(_, :nil), do: :nil
  def take(n, {:cons, x, xs}) when n > 0, do: {:cons, x, take(n - 1, xs)}

  # Drop first n elements
  def drop(0, xs), do: xs
  def drop(_, :nil), do: :nil
  def drop(n, {:cons, _, xs}) when n > 0, do: drop(n - 1, xs)

  # Any - check if any element satisfies predicate
  def any(_p, :nil), do: false
  def any(p, {:cons, x, xs}) do
    if p.(x), do: true, else: any(p, xs)
  end
  def any(p, list) when is_list(list), do: any(p, from_foldable(list))

  # Map Maybe - map with a function that returns Maybe, keeping only Just values
  def map_maybe(_f, :nil), do: :nil
  def map_maybe(f, {:cons, x, xs}) do
    case f.(x) do
      {:just, v} -> {:cons, v, map_maybe(f, xs)}
      :nothing -> map_maybe(f, xs)
    end
  end
  def map_maybe(f, list) when is_list(list), do: map_maybe(f, from_foldable(list))

  # Take while - take elements while predicate is true
  def take_while(_p, :nil), do: :nil
  def take_while(p, {:cons, x, xs}) do
    if p.(x), do: {:cons, x, take_while(p, xs)}, else: :nil
  end
  def take_while(p, list) when is_list(list), do: take_while(p, from_foldable(list))

  # Range - create a list from start to end (inclusive)
  def range(start, finish) when start > finish, do: :nil
  def range(start, finish), do: {:cons, start, range(start + 1, finish)}

  # To unfoldable - convert linked list to Elixir list (same as to_list)
  def to_unfoldable(list), do: to_list(list)

  # Check if element is in the list
  def elem(_x, :nil), do: false
  def elem(x, {:cons, h, _}) when x == h, do: true
  def elem(x, {:cons, _, t}), do: elem(x, t)
  def elem(x, list) when is_list(list), do: Enum.member?(list, x)
end
