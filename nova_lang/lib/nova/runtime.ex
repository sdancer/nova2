# Nova Runtime Library for Elixir
# Provides Prelude functions expected by generated Nova code

defmodule Nova.Runtime do
  @moduledoc """
  Runtime support for Nova-generated Elixir code.
  """

  # Basic prelude
  def show(x) when is_integer(x), do: Integer.to_string(x)
  def show(x) when is_binary(x), do: inspect(x)
  def show(x) when is_boolean(x), do: Atom.to_string(x)
  def show(x) when is_atom(x), do: Atom.to_string(x)
  def show(x), do: inspect(x)

  def length(list) when is_list(list), do: Kernel.length(list)

  # Either constructors
  def left(x), do: {:left, x}
  def right(x), do: {:right, x}

  # Either bind (for do-notation desugaring)
  def bind({:left, err}, _f), do: {:left, err}
  def bind({:right, x}, f), do: f.(x)
  # Maybe bind
  def bind(:nothing, _f), do: :nothing
  def bind({:just, x}, f), do: f.(x)
  # Effect bind (for IO/Effect monad - thunks)
  # Effect is represented as a 0-arity function (thunk)
  def bind(effect, f) when is_function(effect, 0) do
    fn ->
      result = effect.()
      next_effect = f.(result)
      if is_function(next_effect, 0), do: next_effect.(), else: next_effect
    end
  end
  # Effect represented as a 1-arity function (for log etc)
  def bind(effect, f) when is_function(effect, 1) do
    fn ->
      result = effect.(:unit)
      next_effect = f.(result)
      if is_function(next_effect, 0), do: next_effect.(), else: next_effect
    end
  end
  # Eager IO bind - when the Effect was already executed (returns :ok or :unit)
  # This happens when IO functions like log are called eagerly in Elixir
  def bind(:ok, f), do: f.(:unit)
  def bind(:unit, f), do: f.(:unit)
  # Parser bind
  def bind({:parser, pa}, f) do
    {:parser, fn ts ->
      case pa.(ts) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, a, rest}} ->
          # f might return {:parser, _} or {:right, _} from generic pure
          # Handle both cases
          case f.(a) do
            {:parser, pb} -> pb.(rest)
            {:right, result} -> {:right, {:tuple, result, rest}}
            {:left, err} -> {:left, err}
          end
      end
    end}
  end

  # Parser alternative - try first, if fails try second
  def alt({:parser, p1}, {:parser, p2}) do
    {:parser, fn ts ->
      case p1.(ts) do
        {:right, result} -> {:right, result}
        {:left, _} -> p2.(ts)
      end
    end}
  end
  # Handle case where second arg is pure result (Either) - wrap it as Parser
  def alt({:parser, p1}, {:right, value}) do
    {:parser, fn ts ->
      case p1.(ts) do
        {:right, result} -> {:right, result}
        {:left, _} -> {:right, {:tuple, value, ts}}  # pure value preserves tokens
      end
    end}
  end

  # Semigroup append - works for both strings and lists
  def append(a, b) when is_binary(a) and is_binary(b), do: a <> b
  def append(a, b) when is_list(a) and is_list(b), do: a ++ b
  def append(a, b), do: a <> b  # Default to string concat

  # Maybe constructors
  def just(x), do: {:just, x}
  def nothing(), do: :nothing

  # from_maybe - get value from Maybe or use default
  def from_maybe(default, :nothing), do: default
  def from_maybe(_default, {:just, x}), do: x

  # Tuple constructor
  def tuple(a, b), do: {:tuple, a, b}

  # Tuple accessors
  def fst({:tuple, a, _}), do: a
  def snd({:tuple, _, b}), do: b

  # Identity
  def identity(x), do: x

  # Const - curried version for 1-arity call, and direct 2-arg version for code that calls const(a, b)
  def const(a), do: fn _b -> a end
  def const(a, _b), do: a
  def const_(a, _b), do: a

  # Maybe predicates
  def is_just({:just, _}), do: true
  def is_just(:nothing), do: false
  def is_nothing(:nothing), do: true
  def is_nothing({:just, _}), do: false

  # 1-arity from_maybe (curried version) - for use in code like `from_maybe(default).(maybe_val)`
  def from_maybe(default) when not is_tuple(default) or elem(default, 0) != :just do
    fn
      :nothing -> default
      {:just, x} -> x
    end
  end

  # Function composition
  def compose(f, g), do: fn x -> f.(g.(x)) end

  # Foldable - support both curried (f.(a).(b)) and uncurried (f.(a, b)) styles
  def foldr(_f, acc, []), do: acc
  def foldr(_f, acc, :nil), do: acc
  def foldr(_f, acc, :nil_), do: acc
  def foldr(f, acc, [h | t]) do
    case :erlang.fun_info(f, :arity) do
      {:arity, 2} -> f.(h, foldr(f, acc, t))
      {:arity, 1} -> f.(h).(foldr(f, acc, t))
    end
  end
  def foldr(f, acc, {:cons, h, t}) do
    case :erlang.fun_info(f, :arity) do
      {:arity, 2} -> f.(h, foldr(f, acc, t))
      {:arity, 1} -> f.(h).(foldr(f, acc, t))
    end
  end

  def foldl(_f, acc, []), do: acc
  def foldl(_f, acc, :nil), do: acc
  def foldl(_f, acc, :nil_), do: acc
  def foldl(f, acc, [h | t]) do
    case :erlang.fun_info(f, :arity) do
      {:arity, 2} -> foldl(f, f.(acc, h), t)
      {:arity, 1} -> foldl(f, f.(acc).(h), t)
    end
  end
  def foldl(f, acc, {:cons, h, t}) do
    case :erlang.fun_info(f, :arity) do
      {:arity, 2} -> foldl(f, f.(acc, h), t)
      {:arity, 1} -> foldl(f, f.(acc).(h), t)
    end
  end
  # Fold over Elixir map values
  def foldl(f, acc, map) when is_map(map) do
    case :erlang.fun_info(f, :arity) do
      {:arity, 2} -> Enum.reduce(Map.values(map), acc, fn v, a -> f.(a, v) end)
      {:arity, 1} -> Enum.reduce(Map.values(map), acc, fn v, a -> f.(a).(v) end)
    end
  end

  # 1-arity fold_m (curried version) - for use like fold_m(f).(acc).(list)
  def fold_m(f) when is_function(f), do: fn acc -> fn list -> fold_m(f, acc, list) end end
  def fold_m(f, acc, []), do: {:right, acc}
  def fold_m(f, acc, [h | t]) do
    result = case :erlang.fun_info(f, :arity) do
      {:arity, 2} -> f.(acc, h)
      {:arity, 1} -> f.(acc).(h)
    end
    case result do
      {:right, new_acc} -> fold_m(f, new_acc, t)
      {:left, err} -> {:left, err}
    end
  end

  # Array/List functions
  # 1-arity map (curried version) - for use like map(f).(list)
  def map(f) when is_function(f, 1), do: fn list -> map(f, list) end
  def map(_f, []), do: []
  def map(f, [h | t]), do: [f.(h) | map(f, t)]
  # Map over PureScript-style linked lists
  def map(_f, :nil), do: :nil
  def map(_f, :nil_), do: :nil_
  def map(f, {:cons, h, t}), do: {:cons, f.(h), map(f, t)}
  # Map over Maybe
  def map(_f, :nothing), do: :nothing
  def map(f, {:just, x}), do: {:just, f.(x)}
  # Map over Parser
  def map(f, {:parser, pa}) do
    {:parser, fn ts ->
      case pa.(ts) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, a, rest}} -> {:right, {:tuple, f.(a), rest}}
      end
    end}
  end
  # Map over Elixir map values (for PureScript Map.map)
  def map(f, map) when is_map(map) do
    Map.new(map, fn {k, v} -> {k, f.(v)} end)
  end

  # fmap - Functor map operation
  # For lists: map over elements
  # For functions: function composition (f <$> g = f . g)
  def fmap(f, x) when is_list(x), do: map(f, x)
  def fmap(f, g) when is_function(g, 1), do: fn a -> f.(g.(a)) end
  def fmap(f, x), do: map(f, x)
  def fmap(f) when is_function(f, 1), do: fn x -> fmap(f, x) end

  # seq - sequence two parsers, keeping second result (Applicative *>)
  def seq({:parser, p1}, {:parser, p2}) do
    {:parser, fn ts ->
      case p1.(ts) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, _, rest}} -> p2.(rest)
      end
    end}
  end

  # seqL - sequence two parsers, keeping first result (Applicative <*)
  def seq_l({:parser, p1}, {:parser, p2}) do
    {:parser, fn ts ->
      case p1.(ts) do
        {:left, err} -> {:left, err}
        {:right, {:tuple, a, rest1}} ->
          case p2.(rest1) do
            {:left, err} -> {:left, err}
            {:right, {:tuple, _, rest2}} -> {:right, {:tuple, a, rest2}}
          end
      end
    end}
  end

  # applySecond / applyFirst - alias for seq/seq_l
  def apply_second(p1, p2), do: seq(p1, p2)
  def apply_first(p1, p2), do: seq_l(p1, p2)

  def filter(_f, []), do: []
  def filter(f, [h | t]) do
    if f.(h), do: [h | filter(f, t)], else: filter(f, t)
  end

  def intercalate(_sep, []), do: ""
  def intercalate(_sep, [x]), do: x
  def intercalate(sep, [h | t]), do: h <> sep <> intercalate(sep, t)

  def zip([], _), do: []
  def zip(_, []), do: []
  def zip([h1 | t1], [h2 | t2]), do: [{:tuple, h1, h2} | zip(t1, t2)]

  def pure(x), do: {:right, x}

  # Effect.Console functions
  def log(msg), do: IO.puts(msg)
  def log_show(val), do: IO.puts(inspect(val))

  # Test assertion functions
  def assert(true), do: :unit
  def assert(false), do: raise "Assertion failed"
  def assert_prime(true), do: :unit
  def assert_prime(false), do: raise "Assertion failed"
  # PureScript's assertEqual takes { expected, actual } record
  def assert_equal(%{expected: expected, actual: actual}) when expected == actual, do: :unit
  def assert_equal(%{expected: expected, actual: actual}), do: raise "Assertion failed: expected #{inspect(expected)}, got #{inspect(actual)}"
  # Also support two-argument form
  def assert_equal(a, b) when a == b, do: :unit
  def assert_equal(a, b), do: raise "Assertion failed: #{inspect(a)} != #{inspect(b)}"
  def assert_throws(f) do
    try do
      f.()
      raise "Expected throw but none occurred"
    rescue
      _ -> :unit
    catch
      _ -> :unit
    end
  end

  # Safe coercion (identity for runtime)
  def coerce(x), do: x
  def unsafe_coerce(x), do: x
  def unsafe_partial(f), do: f.()
  def unsafe_crash_with(msg), do: raise msg

  # Reflection
  def reflect_symbol(_), do: "symbol"
  def proxy(), do: :proxy

  # FFI helpers
  def run_fn2(f, a, b), do: f.(a).(b)
  def mk_fn2(f), do: fn a -> fn b -> f.(a, b) end end

  # Prelude extras
  def negate(x), do: -x
  def flip(f), do: fn b -> fn a -> f.(a).(b) end end
  def mempty(), do: []
  def discard(_x), do: fn y -> y end
  def unit(), do: :unit

  # Parser-specific pure - wraps value in Parser that preserves token stream
  # pure a = Parser \ts -> Right (Tuple a ts)
  def pure_parser(x) do
    {:parser, fn ts -> {:right, {:tuple, x, ts}} end}
  end

  def otherwise(), do: true

  # Fix combinator for recursive anonymous functions
  # Usage: fix(fn f -> fn x -> ... f.(y) ... end end)
  # The outer function receives the recursive function as argument
  # Optimized using Z-combinator style: create recursive ref once
  def fix(f) do
    # Create the recursive function once by using process dictionary for self-reference
    # This avoids creating new closures on every recursive call
    rec = make_ref()
    g = f.(fn x -> Process.get(rec).(x) end)
    Process.put(rec, g)
    g
  end

  # Multi-arity fix combinators - curried versions
  def fix2(f) do
    rec = make_ref()
    g = f.(fn a -> fn b -> Process.get(rec).(a).(b) end end)
    Process.put(rec, g)
    g
  end

  def fix3(f) do
    rec = make_ref()
    g = f.(fn a -> fn b -> fn c -> Process.get(rec).(a).(b).(c) end end end)
    Process.put(rec, g)
    g
  end

  def fix4(f) do
    rec = make_ref()
    g = f.(fn a -> fn b -> fn c -> fn d -> Process.get(rec).(a).(b).(c).(d) end end end end)
    Process.put(rec, g)
    g
  end

  def fix5(f) do
    rec = make_ref()
    g = f.(fn a -> fn b -> fn c -> fn d -> fn e -> Process.get(rec).(a).(b).(c).(d).(e) end end end end end)
    Process.put(rec, g)
    g
  end
end

defmodule Nova.Array do
  def reverse(list), do: Enum.reverse(list)
  def head([h | _]), do: {:just, h}
  def head([]), do: :nothing
  def tail([_ | t]), do: {:just, t}
  def tail([]), do: :nothing
  def uncons([]), do: :nothing
  def uncons([h | t]), do: {:just, %{head: h, tail: t}}
  def take(n, list), do: Enum.take(list, n)
  def drop(n, list), do: Enum.drop(list, n)
  def drop_while(f, list), do: Enum.drop_while(list, f)
  def elem(x, list), do: Enum.member?(list, x)
  def cons(x, list), do: [x | list]
  def snoc(list, x), do: list ++ [x]
  def concat(lists), do: List.flatten(lists)
  def span(f, list) do
    {init, rest} = Enum.split_while(list, f)
    %{init: init, rest: rest}
  end
  def length(list), do: Kernel.length(list)
  def null(list), do: list == []
  def last([x]), do: {:just, x}
  def last([_ | t]), do: last(t)
  def last([]), do: :nothing
  def find(f, list) do
    case Enum.find(list, f) do
      nil -> :nothing
      x -> {:just, x}
    end
  end
  # elem_index - find index of element in list (previously named index)
  def elem_index(x, list) do
    case Enum.find_index(list, fn el -> el == x end) do
      nil -> :nothing
      idx -> {:just, idx}
    end
  end
  # index - get element at index (PureScript Array.index)
  def index(list, idx) when is_list(list) and is_integer(idx) do
    case Enum.at(list, idx) do
      nil -> :nothing
      x -> {:just, x}
    end
  end
  def filter(f, list), do: Enum.filter(list, f)
  def map(f, list), do: Enum.map(list, f)
  def map_maybe(f, list) do
    Enum.flat_map(list, fn x ->
      case f.(x) do
        {:just, v} -> [v]
        :nothing -> []
      end
    end)
  end
  def zip_with(f, l1, l2), do: Enum.zip_with(l1, l2, fn a, b -> f.(a).(b) end)
  # Support both curried (f.(x).(y)) and uncurried (f.(x, y)) styles
  def foldr(f, acc, list) do
    case :erlang.fun_info(f, :arity) do
      {:arity, 2} -> List.foldr(list, acc, fn x, a -> f.(x, a) end)
      {:arity, 1} -> List.foldr(list, acc, fn x, a -> f.(x).(a) end)
    end
  end
  def foldl(f, acc, list) do
    case :erlang.fun_info(f, :arity) do
      {:arity, 2} -> List.foldl(list, acc, fn x, a -> f.(a, x) end)
      {:arity, 1} -> List.foldl(list, acc, fn x, a -> f.(a).(x) end)
    end
  end
  def sort_by(f, list), do: Enum.sort_by(list, f)
  def update_at(idx, f, list), do: List.update_at(list, idx, f)
  def delete_at(idx, list), do: List.delete_at(list, idx)
  def range(a, b), do: Enum.to_list(a..b)
  def replicate(n, x), do: List.duplicate(x, n)
  # Support both curried (f.(i).(x)) and uncurried (f.(i, x)) styles
  def map_with_index(f, list) do
    case :erlang.fun_info(f, :arity) do
      {:arity, 2} -> Enum.with_index(list, fn x, i -> f.(i, x) end)
      {:arity, 1} -> Enum.with_index(list, fn x, i -> f.(i).(x) end)
    end
  end
  def zip(l1, l2), do: Enum.zip_with(l1, l2, fn a, b -> {:tuple, a, b} end)
  # concat_map (flatMap)
  def concat_map(f, list), do: Enum.flat_map(list, f)
  # partition
  def partition(f, list) do
    {yes, no} = Enum.split_with(list, f)
    %{yes: yes, no: no}
  end
  # all (every)
  def all(f, list), do: Enum.all?(list, f)
  # any
  def any(f, list), do: Enum.any?(list, f)
  # init (all but last)
  def init([]), do: :nothing
  def init([_]), do: {:just, []}
  def init([h | t]) do
    case init(t) do
      {:just, rest} -> {:just, [h | rest]}
      :nothing -> {:just, []}
    end
  end
  # Convert array to PureScript List (Cons/Nil)
  def to_unfoldable([]), do: :nil
  def to_unfoldable([h | t]), do: {:cons, h, to_unfoldable(t)}
  # Convert PureScript List to array
  def from_foldable(:nil), do: []
  def from_foldable(:nil_), do: []
  def from_foldable({:cons, h, t}), do: [h | from_foldable(t)]
  def from_foldable(list) when is_list(list), do: list  # Already an Elixir list

  # nub - remove duplicates (using Ord)
  def nub(list), do: Enum.uniq(list)

  # nub_by_eq - remove duplicates using custom equality function
  def nub_by_eq(eq_fn, list) do
    Enum.reduce(list, [], fn x, acc ->
      if Enum.any?(acc, fn y -> eq_fn.(x).(y) end) do
        acc
      else
        acc ++ [x]
      end
    end)
  end

  # find_index - find index of first element matching predicate
  def find_index(f, list) do
    case Enum.find_index(list, f) do
      nil -> :nothing
      idx -> {:just, idx}
    end
  end

  # sort - sort a list using default ordering
  def sort(list), do: Enum.sort(list)

  # unzip - convert list of tuples to tuple of lists
  def unzip(list) do
    {as, bs} = Enum.unzip(Enum.map(list, fn {:tuple, a, b} -> {a, b} end))
    {:tuple, as, bs}
  end

  # intercalate - join list with separator
  def intercalate(sep, list), do: Enum.join(list, sep)

  # lookup - look up key in list of pairs
  def lookup(k, list) do
    case Enum.find(list, fn {:tuple, key, _} -> key == k end) do
      nil -> :nothing
      {:tuple, _, v} -> {:just, v}
    end
  end

  # insert/delete/member - map-like operations for association lists
  def insert(k, v, list), do: [{:tuple, k, v} | Enum.reject(list, fn {:tuple, key, _} -> key == k end)]
  def delete(k, list), do: Enum.reject(list, fn {:tuple, key, _} -> key == k end)
  def member(k, list), do: Enum.any?(list, fn {:tuple, key, _} -> key == k end)

  # singleton - create a single-element list
  def singleton(x), do: [x]

  # empty - empty list
  def empty(), do: []
end

defmodule Nova.Map do
  def empty(), do: %{}
  def singleton(k, v), do: %{k => v}
  def insert(k, v, map), do: Map.put(map, k, v)
  def delete(k, map), do: Map.delete(map, k)
  def map(f, map), do: Map.new(map, fn {k, v} -> {k, f.(v)} end)
  def lookup(k, map) do
    case Map.fetch(map, k) do
      {:ok, v} -> {:just, v}
      :error -> :nothing
    end
  end
  def member(k, map), do: Map.has_key?(map, k)
  def keys(map), do: Map.keys(map)
  def values(map), do: Map.values(map)
  def union(m1, m2), do: Map.merge(m1, m2)
  def from_foldable(list) do
    Enum.reduce(list, %{}, fn {:tuple, k, v}, acc -> Map.put(acc, k, v) end)
  end
  def to_unfoldable(map) do
    Enum.map(map, fn {k, v} -> {:tuple, k, v} end)
  end
  # Map.mapMaybe - filter and transform values
  def map_maybe(f, map) do
    Enum.reduce(map, %{}, fn {k, v}, acc ->
      case f.(v) do
        {:just, v2} -> Map.put(acc, k, v2)
        :nothing -> acc
      end
    end)
  end
end

defmodule Nova.Set do
  def empty(), do: MapSet.new()
  def singleton(x), do: MapSet.new([x])
  def insert(x, set), do: MapSet.put(set, x)
  def member(x, set), do: MapSet.member?(set, x)
  def delete(x, set), do: MapSet.delete(set, x)
  def union(s1, s2), do: MapSet.union(s1, s2)
  def difference(s1, s2), do: MapSet.difference(s1, s2)
  def intersection(s1, s2), do: MapSet.intersection(s1, s2)
  def from_foldable(list), do: MapSet.new(list)
  def to_unfoldable(set), do: MapSet.to_list(set)
  def size(set), do: MapSet.size(set)
  def is_empty(set), do: MapSet.size(set) == 0
  def find_min(set) do
    case MapSet.to_list(set) do
      [] -> :nothing
      list -> {:just, Enum.min(list)}
    end
  end
  def map_maybe(f, set) do
    set
    |> MapSet.to_list()
    |> Enum.flat_map(fn x ->
      case f.(x) do
        {:just, v} -> [v]
        :nothing -> []
      end
    end)
    |> MapSet.new()
  end
end

defmodule Nova.String do
  def length(s), do: String.length(s)
  def char_at(n, s) do
    case String.at(s, n) do
      nil -> :nothing
      <<cp::utf8>> -> {:just, cp}
    end
  end
  def contains({:pattern, p}, s), do: String.contains?(s, p)
  def take(n, s), do: String.slice(s, 0, n)
  def drop(n, s), do: String.slice(s, n..-1//1)
  def split({:pattern, p}, s), do: String.split(s, p)
  def split(pattern, s) when is_binary(pattern), do: String.split(s, pattern)
  def join_with(sep, list), do: Enum.join(list, sep)
  def replace_all({:pattern, p}, {:replacement, r}, s), do: String.replace(s, p, r)
  # 2-arg version for curried calls
  def replace_all({:pattern, p}, s) when is_binary(s), do: fn {:replacement, r} -> String.replace(s, p, r) end
  def replace_all({:pattern, p}, {:replacement, r}), do: fn s -> String.replace(s, p, r) end

  # index_of - find index of pattern in string
  def index_of({:pattern, p}, s) do
    case :binary.match(s, p) do
      {idx, _} -> {:just, idx}
      :nomatch -> :nothing
    end
  end

  # to_upper - convert to uppercase
  def to_upper(s), do: String.upcase(s)
  def pattern(p), do: {:pattern, p}
  def replacement(r), do: {:replacement, r}
  def last_index_of({:pattern, p}, s) do
    case :binary.matches(s, p) do
      [] -> :nothing
      matches -> {:just, elem(List.last(matches), 0)}
    end
  end
  def trim(s), do: String.trim(s)
  def to_code_point_array(s), do: String.to_charlist(s)
  def to_char_array(s), do: String.to_charlist(s)  # CU.toCharArray
  def singleton(cp), do: <<cp::utf8>>
  def from_char_array(chars), do: List.to_string(chars)
  def to_lower(s), do: String.downcase(s)
  def strip_prefix({:pattern, prefix}, s) do
    case String.split(s, prefix, parts: 2) do
      ["", rest] -> {:just, rest}
      _ -> :nothing
    end
  end
  def to_int(s) do
    case Integer.parse(s) do
      {n, ""} -> {:just, n}
      _ -> :nothing
    end
  end
  def to_float(s) do
    case Float.parse(s) do
      {n, ""} -> {:just, n}
      # Also handle integers (e.g., "42" should parse as 42.0)
      _ ->
        case Integer.parse(s) do
          {n, ""} -> {:just, n * 1.0}
          _ -> :nothing
        end
    end
  end
end
