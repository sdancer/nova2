# FFI module for EffFn test
# Corresponds to EffFn.js: export var add3 = function (a,b,c) { return a + b + c; };

defmodule Nova.FFI.EffFn do
  # Note: This is an EffectFn3, meaning it takes 3 args at once (not curried)
  def add3(a, b, c), do: a <> b <> c
end
