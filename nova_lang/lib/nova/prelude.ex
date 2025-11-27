defmodule Nova.Prelude do
  import Nova.Runtime.Prelude

  # Data type: Maybe
  def nothing(), do: :nothing
  def just(arg0), do: {:just, arg0}

  # Data type: Either
  def left(arg0), do: {:left, arg0}
  def right(arg0), do: {:right, arg0}

  # Data type: Unit
  def unit(), do: :unit

  # Data type: Tuple
  def tuple(arg0, arg1), do: {:tuple, arg0, arg1}

  def fst({:tuple, a, _}), do: a
  def snd({:tuple, _, b}), do: b

  def identity(x), do: x
  def const(x, _), do: x
  def compose(f, g, x), do: f.(g.(x))
  def flip(f, b, a), do: f.(a).(b)

  def from_maybe(def_, :nothing), do: def_
  def from_maybe(_, {:just, x}), do: x

  def is_just({:just, _}), do: true
  def is_just(:nothing), do: false

  def is_nothing(:nothing), do: true
  def is_nothing({:just, _}), do: false

  def maybe(def_, _, :nothing), do: def_
  def maybe(_, f, {:just, x}), do: f.(x)

  def either(f, _, {:left, a}), do: f.(a)
  def either(_, g, {:right, b}), do: g.(b)

  def is_left({:left, _}), do: true
  def is_left({:right, _}), do: false

  def is_right({:right, _}), do: true
  def is_right({:left, _}), do: false

  def otherwise(), do: true

  def show(x), do: show_impl().(x)
  def negate(x), do: Nova.Runtime.Prelude.negate().(x)
  def map(f, xs), do: map_impl().(f).(xs)
  def filter(f, xs), do: filter_impl().(f).(xs)
  def length(xs), do: length_impl().(xs)
  def append(xs, ys), do: append_impl().(xs).(ys)
  def foldl(f, acc, xs), do: foldl_impl().(f).(acc).(xs)
  def foldr(f, acc, xs), do: foldr_impl().(f).(acc).(xs)
end
