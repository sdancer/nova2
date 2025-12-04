defmodule Nova.Compiler.TypeChecker do
  # import Prelude

  # import Data.Either

  # import Data.Maybe

  # import Data.Tuple

  # import Data.Array

  # import Data.Array

  # import Data.List

  # import Data.List

  # import Data.Foldable

  # import Data.Map

  # import Data.Set

  # import Data.String

  # import Nova.Compiler.Types

  # import Nova.Compiler.Ast

  # import Nova.Compiler.Unify

  # Data type: TCError
  def unify_err(arg0), do: {:unify_err, arg0}
  def unbound_variable(arg0), do: {:unbound_variable, arg0}
  def not_implemented(arg0), do: {:not_implemented, arg0}

  # instance Show tcerror()
end
