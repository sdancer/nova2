# FFI module for FunWithFunDeps test
# Corresponds to FunWithFunDeps.js

defmodule Nova.FFI.FunWithFunDeps do
  # fnil: forall e. FVect Z e - an empty vector
  def fnil(), do: []

  # fcons: forall n e. e -> FVect n e -> FVect (S n) e
  # Curried: fcons(hd) -> (tl -> [hd | tl])
  def fcons(hd) do
    fn tl -> [hd | tl] end
  end

  # fappendImpl: forall l r o e. FVect l e -> FVect r e -> FVect o e
  # Curried: fappendImpl(left) -> (right -> left ++ right)
  def fappend_impl(left) do
    fn right -> left ++ right end
  end

  # fflattenImpl: forall f s t o. FVect f (FVect s t) -> FVect o t
  # Flatten a vector of vectors
  def fflatten_impl(v), do: List.flatten(v)

  # ftoArray: forall n e. FVect n e -> Array e
  # Convert vector to array (which is the same in Elixir)
  def fto_array(vect), do: vect
end
