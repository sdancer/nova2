# FFI module for ESFFIFunctionFunction test
# Corresponds to ESFFIFunctionFunction.js: export function functionName(a) { return a; }

defmodule Nova.FFI.ESFFIFunctionFunction do
  def function_name(a), do: a
end
