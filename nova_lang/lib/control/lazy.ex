# Control.Lazy module for deferred computation
# Used for recursive parser definitions

defmodule Control.Lazy do
  @moduledoc """
  Provides lazy evaluation support, particularly for recursive parser definitions.
  """

  # defer - delay evaluation of a thunk
  # Used in parser combinators for recursive grammars
  # Accept 0-arity thunk (standard)
  def defer(thunk) when is_function(thunk, 0) do
    {:parser, fn ts ->
      {:parser, p} = thunk.()
      p.(ts)
    end}
  end

  # Accept 1-arity thunk (PureScript's \_ -> expr pattern)
  # The argument is ignored, matching PureScript's defer \_ -> expr
  def defer(thunk) when is_function(thunk, 1) do
    {:parser, fn ts ->
      {:parser, p} = thunk.(nil)
      p.(ts)
    end}
  end

  # For non-parser lazy values
  def force({:lazy, thunk}), do: thunk.()
  def force(x), do: x
end
