# Effect.Console module for Nova
# Provides console logging functions for Nova-generated code

defmodule Effect.Console do
  @moduledoc """
  Console logging functions for Nova Effect system.
  These return Effect (thunks) for lazy IO evaluation.
  """

  # log :: String -> Effect Unit
  # Returns a thunk that prints the message when executed
  def log(msg) do
    fn ->
      IO.puts(msg)
      :unit
    end
  end

  # logShow :: Show a => a -> Effect Unit
  # Returns a thunk that prints the inspected value when executed
  def log_show(x) do
    fn ->
      IO.puts(Nova.Runtime.show(x))
      :unit
    end
  end
end
