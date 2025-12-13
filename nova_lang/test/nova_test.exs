defmodule NovaTest do
  use ExUnit.Case

  test "runtime module loads" do
    # Just verify the runtime module is accessible
    assert Nova.Runtime.not(false) == true
  end
end
