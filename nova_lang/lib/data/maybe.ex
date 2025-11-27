defmodule Data.Maybe do
  # Data type: Maybe
  def nothing(), do: :nothing
  def just(arg0), do: {:just, arg0}

  def from_maybe(def_, :nothing) do
    def_
  end

  def from_maybe(_, ({:just, x})) do
    x
  end

  def is_just(({:just, _})) do
    true
  end

  def is_just(:nothing) do
    false
  end

  def is_nothing(:nothing) do
    true
  end

  def is_nothing(({:just, _})) do
    false
  end

  def maybe(def_, _, :nothing) do
    def_
  end

  def maybe(_, f, ({:just, x})) do
    f.(x)
  end

  def map(_, :nothing) do
    :nothing
  end

  def map(f, ({:just, x})) do
    {:just, f.(x)}
  end

  def pure(x) do
    {:just, x}
  end

  def bind(:nothing, _) do
    :nothing
  end

  def bind(({:just, x}), f) do
    f.(x)
  end
end
