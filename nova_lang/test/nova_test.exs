defmodule NovaTest do
  use ExUnit.Case
  doctest Nova

  test "compile simple function" do
    source = """
    module Test where

    add x y = x + y
    """
    assert {:ok, code} = Nova.compile(source)
    assert code =~ "defmodule Test"
    assert code =~ "def add"
  end
end
