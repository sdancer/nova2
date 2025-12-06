defmodule Prelude do
  @moduledoc "Prelude - standard library functions"

  # Data constructors from Nova.Prelude
  defdelegate nothing(), to: Nova.Prelude
  defdelegate just(arg0), to: Nova.Prelude
  defdelegate left(arg0), to: Nova.Prelude
  defdelegate right(arg0), to: Nova.Prelude
  defdelegate unit(), to: Nova.Prelude
  defdelegate tuple(arg0, arg1), to: Nova.Prelude
  defdelegate fst(t), to: Nova.Prelude
  defdelegate snd(t), to: Nova.Prelude
  defdelegate identity(x), to: Nova.Prelude
  defdelegate from_maybe(def_, m), to: Nova.Prelude
  defdelegate is_just(m), to: Nova.Prelude
  defdelegate is_nothing(m), to: Nova.Prelude
  defdelegate maybe(def_, f, m), to: Nova.Prelude
  defdelegate either(f, g, e), to: Nova.Prelude
  defdelegate is_left(e), to: Nova.Prelude
  defdelegate is_right(e), to: Nova.Prelude
  defdelegate otherwise(), to: Nova.Prelude

  # Core runtime functions
  defdelegate show(x), to: Nova.Runtime
  defdelegate map(f, xs), to: Nova.Runtime
  defdelegate filter(f, xs), to: Nova.Array
  defdelegate length(xs), to: Nova.Array
  defdelegate append(xs, ys), to: Nova.Runtime
  defdelegate foldl(f, acc, xs), to: Nova.Array
  defdelegate foldr(f, acc, xs), to: Nova.Array
  defdelegate fold_m(f, acc, xs), to: Nova.Runtime
  defdelegate pure(x), to: Nova.Runtime
  defdelegate bind(m, f), to: Nova.Runtime
  defdelegate zip(xs, ys), to: Nova.Array
  defdelegate fmap(f, x), to: Nova.Runtime
  defdelegate alt(x, y), to: Nova.Runtime
  defdelegate seq(x, y), to: Nova.Runtime
  defdelegate compose(f, g), to: Nova.Runtime
  defdelegate negate(x), to: Nova.Runtime
  defdelegate flip(f), to: Nova.Runtime
  def const(a, _b), do: a
  def const(a), do: fn _b -> a end

  # Array functions from Nova.Array
  defdelegate head(xs), to: Nova.Array
  defdelegate tail(xs), to: Nova.Array
  defdelegate reverse(xs), to: Nova.Array
  defdelegate concat(xs), to: Nova.Array
  defdelegate concat_map(f, xs), to: Nova.Array
  defdelegate take(n, xs), to: Nova.Array
  defdelegate drop(n, xs), to: Nova.Array
  defdelegate elem(x, xs), to: Nova.Array
  defdelegate find(f, xs), to: Nova.Array
  defdelegate find_index(f, xs), to: Nova.Array
  defdelegate any(f, xs), to: Nova.Array
  defdelegate all(f, xs), to: Nova.Array
  defdelegate zip_with(f, xs, ys), to: Nova.Array
  defdelegate unzip(xs), to: Nova.Array
  defdelegate sort(xs), to: Nova.Array
  defdelegate sort_by(f, xs), to: Nova.Array
  defdelegate nub(xs), to: Nova.Array
  defdelegate cons(x, xs), to: Nova.Array
  defdelegate snoc(xs, x), to: Nova.Array
  defdelegate replicate(n, x), to: Nova.Array
  defdelegate null(xs), to: Nova.Array

  # Map functions from Nova.Map
  defdelegate lookup(k, m), to: Nova.Map
  defdelegate insert(k, v, m), to: Nova.Map
  defdelegate delete(k, m), to: Nova.Map
  defdelegate member(k, m), to: Nova.Map
  def singleton(x) when is_list(x), do: x
  def singleton(x), do: [x]
  defdelegate empty(), to: Nova.Map

  # String functions from Nova.String
  defdelegate split(sep, s), to: Nova.String
  defdelegate join_with(sep, xs), to: Nova.String
  defdelegate trim(s), to: Nova.String
  defdelegate to_lower(s), to: Nova.String
  defdelegate to_upper(s), to: Nova.String
  defdelegate contains(sub, s), to: Nova.String
  defdelegate index_of(sub, s), to: Nova.String
  defdelegate replace_all(p, r, s), to: Nova.String
  defdelegate char_at(i, s), to: Nova.String
  defdelegate to_char_array(s), to: Nova.String
  defdelegate from_char_array(xs), to: Nova.String
  defdelegate intercalate(sep, xs), to: Nova.String, as: :join_with

  # seq_left - parser combinator
  defdelegate seq_left(x, y), to: Nova.Runtime, as: :seq_l
end
