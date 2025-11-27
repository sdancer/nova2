defmodule Data.String do
  import Nova.Runtime.String

  def length(s), do: length_impl().(s)
  def take(n, s), do: take_impl().(n).(s)
  def drop(n, s), do: drop_impl().(n).(s)
  def char_at(n, s), do: char_at_impl().(n).(s)
  def contains(pattern, s), do: contains_impl().(pattern).(s)
  def split(sep, s), do: split_impl().(sep).(s)
  def join_with(sep, xs), do: join_with_impl().(sep).(xs)
  def replace_all(pattern, replacement, s), do: replace_all_impl().(pattern).(replacement).(s)
  def trim(s), do: trim_impl().(s)
  def to_lower(s), do: to_lower_impl().(s)
  def to_upper(s), do: to_upper_impl().(s)
  def strip_prefix(prefix, s), do: strip_prefix_impl().(prefix).(s)
  def last_index_of(pattern, s), do: last_index_of_impl().(pattern).(s)
  def to_int(s), do: to_int_impl().(s)
  def to_code_point_array(s), do: to_code_point_array_impl().(s)
  def singleton(cp), do: singleton_impl().(cp)
  def from_char_array(cs), do: from_char_array_impl().(cs)
end
