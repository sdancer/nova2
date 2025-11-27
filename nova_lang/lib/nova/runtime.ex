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

  # Const
  def const_(a, _b), do: a

  # Function composition
  def compose(f, g), do: fn x -> f.(g.(x)) end

  # Foldable - support both curried (f.(a).(b)) and uncurried (f.(a, b)) styles
  def foldr(f, acc, []), do: acc
  def foldr(f, acc, [h | t]) do
    case :erlang.fun_info(f, :arity) do
      {:arity, 2} -> f.(h, foldr(f, acc, t))
      {:arity, 1} -> f.(h).(foldr(f, acc, t))
    end
  end

  def foldl(f, acc, []), do: acc
  def foldl(f, acc, [h | t]) do
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
  def map(f, []), do: []
  def map(f, [h | t]), do: [f.(h) | map(f, t)]
  # Map over Maybe
  def map(_f, :nothing), do: :nothing
  def map(f, {:just, x}), do: {:just, f.(x)}
  # Map over Elixir map values (for PureScript Map.map)
  def map(f, map) when is_map(map) do
    Map.new(map, fn {k, v} -> {k, f.(v)} end)
  end

  def filter(f, []), do: []
  def filter(f, [h | t]) do
    if f.(h), do: [h | filter(f, t)], else: filter(f, t)
  end

  def intercalate(sep, []), do: ""
  def intercalate(sep, [x]), do: x
  def intercalate(sep, [h | t]), do: h <> sep <> intercalate(sep, t)

  def zip([], _), do: []
  def zip(_, []), do: []
  def zip([h1 | t1], [h2 | t2]), do: [{:tuple, h1, h2} | zip(t1, t2)]

  def pure(x), do: {:right, x}

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
  def index(x, list) do
    case Enum.find_index(list, fn el -> el == x end) do
      nil -> :nothing
      idx -> {:just, idx}
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
  def from_foldable({:cons, h, t}), do: [h | from_foldable(t)]
  def from_foldable(list) when is_list(list), do: list  # Already an Elixir list
end

defmodule Nova.List do
  # PureScript List operations (Cons/Nil based)
  # Note: `nil` is reserved in Elixir, using unquote trick
  def unquote(:nil)(), do: :nil
  def cons(x, xs), do: {:cons, x, xs}
  def from_foldable(list) when is_list(list) do
    Elixir.List.foldr(list, :nil, fn x, acc -> {:cons, x, acc} end)
  end
  def from_foldable(:nil), do: :nil
  def from_foldable({:cons, _, _} = l), do: l
end

defmodule Nova.Map do
  def empty(), do: %{}
  def singleton(k, v), do: %{k => v}
  def insert(k, v, map), do: Map.put(map, k, v)
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
end

defmodule Nova.String do
  def length(s), do: String.length(s)
  # O(1) character access using binary pattern matching
  def char_at(n, s) when is_binary(s) and n >= 0 do
    case s do
      <<_::binary-size(n), c, _::binary>> -> {:just, c}
      _ -> :nothing
    end
  end
  def char_at(_, _), do: :nothing
  def contains({:pattern, p}, s), do: String.contains?(s, p)
  def take(n, s), do: String.slice(s, 0, n)
  def drop(n, s), do: String.slice(s, n..-1//1)
  def split({:pattern, p}, s), do: String.split(s, p)
  def split(pattern, s) when is_binary(pattern), do: String.split(s, pattern)
  def join_with(sep, list), do: Enum.join(list, sep)
  def replace_all({:pattern, p}, {:replacement, r}, s), do: String.replace(s, p, r)
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
