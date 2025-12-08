# Default FFI module for Main module tests
# This is a placeholder - in real usage, specific FFI modules should be created

defmodule Nova.FFI.Main do
  # ESFFIFunctionConst, ESFFIFunctionFunction, ESFFIFunctionVar all have functionName
  def function_name(a), do: a

  # ESFFIValueConst1, ESFFIValueVar have value
  def value(), do: 1

  # FFIDefaultESExport has default
  def default(), do: "Done"
end
