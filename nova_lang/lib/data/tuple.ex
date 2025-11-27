defmodule Data.Tuple do
  # Data type: Tuple
  def tuple(arg0, arg1), do: {:tuple, arg0, arg1}

  def fst(({:tuple, a, _})) do
    a
  end

  def snd(({:tuple, _, b})) do
    b
  end

  def swap(({:tuple, a, b})) do
    {:tuple, b, a}
  end

  def curry(f, a, b) do
    f.({:tuple, a, b})
  end

  def uncurry(f, ({:tuple, a, b})) do
    f.(a).(b)
  end

  def map(f, ({:tuple, a, b})) do
    {:tuple, a, f.(b)}
  end
end
