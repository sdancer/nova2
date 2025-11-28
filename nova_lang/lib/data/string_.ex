defmodule Data.String do
  @moduledoc "Data.String wrapper - delegates to Nova.String"

  defdelegate length(s), to: Nova.String
  defdelegate trim(s), to: Nova.String
  defdelegate to_lower(s), to: Nova.String
  defdelegate to_int(s), to: Nova.String
  defdelegate to_code_point_array(s), to: Nova.String
  defdelegate singleton(cp), to: Nova.String
  defdelegate from_char_array(cs), to: Nova.String

  def take(n, s), do: Nova.String.take(n, s)
  def drop(n, s), do: Nova.String.drop(n, s)
  def char_at(n, s), do: Nova.String.char_at(n, s)
  def contains(pattern, s), do: Nova.String.contains(pattern, s)
  def split(sep, s), do: Nova.String.split(sep, s)
  def join_with(sep, xs), do: Nova.String.join_with(sep, xs)
  def replace_all(pattern, replacement, s), do: Nova.String.replace_all(pattern, replacement, s)
  def to_upper(s), do: String.upcase(s)
  def strip_prefix(prefix, s), do: Nova.String.strip_prefix(prefix, s)
  def last_index_of(pattern, s), do: Nova.String.last_index_of(pattern, s)
end
