# FFI module for ESFFIFunctionVar test
# Corresponds to ESFFIFunctionVar.js: export var functionName = function (a) { return a; }

defmodule Nova.FFI.ESFFIFunctionVar do
  def function_name(a), do: a
end
