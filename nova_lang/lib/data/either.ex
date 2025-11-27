defmodule Data.Either do
  # Data type: Either
  def left(arg0), do: {:left, arg0}
  def right(arg0), do: {:right, arg0}

  def either(f, _, ({:left, a})) do
    f.(a)
  end

  def either(_, g, ({:right, b})) do
    g.(b)
  end

  def is_left(({:left, _})) do
    true
  end

  def is_left(({:right, _})) do
    false
  end

  def is_right(({:right, _})) do
    true
  end

  def is_right(({:left, _})) do
    false
  end

  def from_left(_, ({:left, a})) do
    a
  end

  def from_left(def_, ({:right, _})) do
    def_
  end

  def from_right(_, ({:right, b})) do
    b
  end

  def from_right(def_, ({:left, _})) do
    def_
  end

  def map(_, ({:left, a})) do
    {:left, a}
  end

  def map(f, ({:right, b})) do
    {:right, f.(b)}
  end

  def pure(x) do
    {:right, x}
  end

  def bind(({:left, a}), _) do
    {:left, a}
  end

  def bind(({:right, b}), f) do
    f.(b)
  end
end
