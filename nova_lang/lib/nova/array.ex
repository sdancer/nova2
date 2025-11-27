defmodule Nova.Array do
  @moduledoc """
  Array operations for Nova runtime.
  Arrays are represented as Elixir lists.
  """

  def head([]), do: :nothing
  def head([x | _]), do: {:just, x}

  def last([]), do: :nothing
  def last(list), do: {:just, List.last(list)}

  def tail([]), do: :nothing
  def tail([_ | rest]), do: {:just, rest}

  def init([]), do: :nothing
  def init(list), do: {:just, Enum.drop(list, -1)}

  def uncons([]), do: :nothing
  def uncons([x | xs]), do: {:just, %{head: x, tail: xs}}

  def length(list), do: Kernel.length(list)

  def null([]), do: true
  def null(_), do: false

  def elem(x, list), do: Enum.member?(list, x)

  def cons(x, list), do: [x | list]

  def snoc(list, x), do: list ++ [x]

  def take(n, list), do: Enum.take(list, n)

  def drop(n, list), do: Enum.drop(list, n)

  def reverse(list), do: Enum.reverse(list)

  def filter(pred, list), do: Enum.filter(list, pred)

  def find(pred, list) do
    case Enum.find(list, pred) do
      nil -> :nothing
      x -> {:just, x}
    end
  end

  def foldl(f, acc, list) do
    Enum.reduce(list, acc, fn x, a -> f.(a).(x) end)
  end

  def foldr(f, acc, list) do
    List.foldr(list, acc, fn x, a -> f.(x).(a) end)
  end

  def map(f, list), do: Enum.map(list, f)

  def map_with_index(f, list) do
    list
    |> Enum.with_index()
    |> Enum.map(fn {x, i} -> f.(i).(x) end)
  end

  def replicate(n, x), do: List.duplicate(x, n)

  def zip(xs, ys), do: Enum.zip(xs, ys) |> Enum.map(fn {a, b} -> {:tuple, a, b} end)

  def drop_while(pred, list), do: Enum.drop_while(list, pred)

  def span(pred, list) do
    {init, rest} = Enum.split_while(list, pred)
    %{init: init, rest: rest}
  end

  def map_maybe(f, list) do
    Enum.flat_map(list, fn x ->
      case f.(x) do
        {:just, v} -> [v]
        :nothing -> []
      end
    end)
  end

  def range(start, stop), do: Enum.to_list(start..stop)

  def any(pred, list), do: Enum.any?(list, pred)

  def all(pred, list), do: Enum.all?(list, pred)

  def concat_map(f, list), do: Enum.flat_map(list, f)

  def concat(lists), do: List.flatten(lists)

  def partition(pred, list) do
    {yes, no} = Enum.split_with(list, pred)
    %{yes: yes, no: no}
  end

  def from_foldable(list), do: list

  def to_unfoldable(list), do: list
end
