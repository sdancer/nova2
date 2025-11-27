defmodule Nova.Map do
  @moduledoc """
  Map operations for Nova runtime.
  Maps are represented as Elixir maps with keys being any term.
  """

  def empty, do: %{}

  def singleton(k, v), do: %{k => v}

  def insert(k, v, map), do: Map.put(map, k, v)

  def lookup(k, map) do
    case Map.fetch(map, k) do
      {:ok, v} -> {:just, v}
      :error -> :nothing
    end
  end

  def member(k, map), do: Map.has_key?(map, k)

  def delete(k, map), do: Map.delete(map, k)

  def keys(map), do: Map.keys(map)

  def values(map), do: Map.values(map)

  def union(m1, m2), do: Map.merge(m1, m2)

  def from_foldable(pairs) do
    Enum.reduce(pairs, %{}, fn {:tuple, k, v}, acc -> Map.put(acc, k, v) end)
  end

  def to_unfoldable(map) do
    Enum.map(map, fn {k, v} -> {:tuple, k, v} end)
  end

  def size(map), do: map_size(map)

  def is_empty(map), do: map_size(map) == 0
end
