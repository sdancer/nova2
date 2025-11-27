defmodule Nova.Set do
  @moduledoc """
  Set operations for Nova runtime.
  Sets are represented as MapSets.
  """

  def empty, do: MapSet.new()

  def singleton(x), do: MapSet.new([x])

  def insert(x, set), do: MapSet.put(set, x)

  def member(x, set), do: MapSet.member?(set, x)

  def delete(x, set), do: MapSet.delete(set, x)

  def union(s1, s2), do: MapSet.union(s1, s2)

  def difference(s1, s2), do: MapSet.difference(s1, s2)

  def intersection(s1, s2), do: MapSet.intersection(s1, s2)

  def from_foldable(list), do: MapSet.new(list)

  def to_unfoldable(set), do: MapSet.to_list(set)

  def map(f, set) do
    set
    |> MapSet.to_list()
    |> Enum.map(f)
    |> MapSet.new()
  end

  def map_maybe(f, set) do
    set
    |> MapSet.to_list()
    |> Enum.flat_map(fn x ->
      case f.(x) do
        {:just, v} -> [v]
        :nothing -> []
      end
    end)
    |> MapSet.new()
  end

  def filter(pred, set) do
    set
    |> MapSet.to_list()
    |> Enum.filter(pred)
    |> MapSet.new()
  end

  def size(set), do: MapSet.size(set)

  def is_empty(set), do: MapSet.size(set) == 0

  def find_min(set) do
    list = MapSet.to_list(set)
    case list do
      [] -> :nothing
      _ -> {:just, Enum.min(list)}
    end
  end
end
