-module(map).
-export([delete/2, empty/0, fromFoldable/1, insert/3, keys/1, lookup/2, 'map'/2, singleton/2, toUnfoldable/1, union/2, values/1]).

%% Empty map
empty() -> #{}.

%% Insert key-value pair
insert(K, V, M) -> maps:put(K, V, M).

%% Delete key
delete(K, M) -> maps:remove(K, M).

%% Get all keys
keys(M) -> maps:keys(M).

%% Lookup key (returns Maybe)
lookup(K, M) ->
    case maps:find(K, M) of
        {ok, V} -> {'_just', V};
        error -> '_nothing'
    end.

%% Create singleton map
singleton(K, V) -> #{K => V}.

%% Union of two maps (right-biased)
union(M1, M2) -> maps:merge(M1, M2).

%% Get all values
values(M) -> maps:values(M).

%% Create map from list of tuples [{_tuple, K, V}]
fromFoldable(List) ->
    lists:foldl(fun({'_tuple', K, V}, Acc) -> maps:put(K, V, Acc) end, #{}, List).

%% Convert map to list of tuples [{_tuple, K, V}]
toUnfoldable(M) ->
    [{'_tuple', K, V} || {K, V} <- maps:to_list(M)].

%% Map a function over values in a map (Functor instance for Map)
'map'(F, M) -> maps:map(fun(_K, V) -> F(V) end, M).
