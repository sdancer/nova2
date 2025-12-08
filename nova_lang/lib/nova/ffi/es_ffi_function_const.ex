# FFI module for ESFFIFunctionConst test
# Corresponds to ESFFIFunctionConst.js: export const functionName = function (a) { return a; }

defmodule Nova.FFI.ESFFIFunctionConst do
  def function_name(a), do: a
end
