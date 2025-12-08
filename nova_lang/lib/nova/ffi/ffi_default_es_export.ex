# FFI module for FFIDefaultESExport test
# Corresponds to FFIDefaultESExport.js:
# var message = "Done";
# export { message as default };

defmodule Nova.FFI.FFIDefaultESExport do
  def default(), do: "Done"
end
