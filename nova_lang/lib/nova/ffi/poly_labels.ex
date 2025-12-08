# FFI module for PolyLabels test
# Corresponds to PolyLabels.js

defmodule Nova.FFI.PolyLabels do
  # unsafeGet: String -> Record r -> a
  # Gets a field from a record by string key
  # Curried: unsafeGet(s) -> (o -> o[s])
  def unsafe_get(s) do
    fn o -> Map.get(o, String.to_atom(s)) end
  end

  # unsafeSet: String -> a -> Record r1 -> Record r2
  # Sets a field in a record by string key
  # Curried: unsafeSet(s) -> (a -> (o -> Map.put(o, s, a)))
  def unsafe_set(s) do
    fn a ->
      fn o -> Map.put(o, String.to_atom(s), a) end
    end
  end
end
