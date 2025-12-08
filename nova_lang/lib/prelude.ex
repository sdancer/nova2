# Prelude module - provides PureScript Prelude functions for compiled code
# This module delegates to Nova.Runtime for actual implementations

defmodule Prelude do
  # Monad operations
  defdelegate pure(value), to: Nova.Runtime
  defdelegate bind(m, f), to: Nova.Runtime

  # Functor operations
  defdelegate map(f, x), to: Nova.Runtime

  # Applicative operations - apply a wrapped function to a wrapped value
  def apply(f, x) when is_function(f, 1), do: f.(x)
  def apply({:just, f}, {:just, x}), do: {:just, f.(x)}
  def apply(:nothing, _), do: :nothing
  def apply(_, :nothing), do: :nothing
  def apply({:right, f}, {:right, x}), do: {:right, f.(x)}
  def apply({:left, e}, _), do: {:left, e}
  def apply(_, {:left, e}), do: {:left, e}

  # Basic operations
  defdelegate show(x), to: Nova.Runtime
  defdelegate identity(x), to: Nova.Runtime
  defdelegate negate(x), to: Nova.Runtime

  # Bounded type class
  defdelegate top(), to: Nova.Runtime
  defdelegate bottom(), to: Nova.Runtime

  # const is needed by generated Nova compiler code
  def const(x, _y), do: x
  def const_(x, _y), do: x
  def flip(f), do: fn a -> fn b -> f.(b).(a) end end

  # Unit
  def unit(), do: :unit

  # String operations (for CodeGenCoreErlang snake_case)
  def to_lower(c) when c >= ?A and c <= ?Z, do: c + 32
  def to_lower(c), do: c

  # Array/List operations (for generated code)
  def reverse(list) when is_list(list), do: Enum.reverse(list)
end
