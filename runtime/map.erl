-module(map).
-export([delete/2, empty/0, insert/3, keys/1, lookup/2, singleton/2, union/2]).

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
