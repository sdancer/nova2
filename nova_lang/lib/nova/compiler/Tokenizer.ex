defmodule Nova.Compiler.Tokenizer do
  # import Prelude

  # import Data.Array

  # import Data.String

  # import Data.String.CodeUnits

  # import Data.Maybe

  # import Data.Either

  # import Data.Tuple

  # Data type: TokenType
  def tok_keyword(), do: :tok_keyword
  def tok_identifier(), do: :tok_identifier
  def tok_number(), do: :tok_number
  def tok_string(), do: :tok_string
  def tok_char(), do: :tok_char
  def tok_operator(), do: :tok_operator
  def tok_delimiter(), do: :tok_delimiter
  def tok_newline(), do: :tok_newline
  def tok_unrecognized(), do: :tok_unrecognized

  # derive instance Eq token_type()

  # @type token :: %{token_type: token_type(), value: string(), line: int(), column: int(), pos: int()}



  def mk_token(token_type, value, line, column, pos) do
    %{token_type: token_type, value: value, line: line, column: column, pos: pos}
  end



  def keywords() do
    ["foreign", "module", "where", "import", "data", "type", "class", "instance", "let", "in", "if", "then", "else", "case", "of", "do", "derive", "newtype", "infixl", "infixr", "infix", "forall"]
  end



  def operators() do
    ["==", "/=", "!=", "<=", ">=", "->", "<-", "::", "++", "++=", ">>=", ">>>", "<<<", ">>", "<<", "&&", "||", "<>", "..", "+", "-", "*", "/", "<", ">", "=", "$", "`", ".", "|", "\\", "&", ":", "@"]
  end



  def is_operator_char(c) do
    c
  end
end
