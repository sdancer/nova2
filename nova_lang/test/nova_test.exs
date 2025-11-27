defmodule NovaTest do
  use ExUnit.Case
  doctest Nova

  test "greets the world" do
    assert Nova.hello() == :world
  end
end
