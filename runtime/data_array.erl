-module(data_array).
-export([intercalate/2, zip/2]).

%% Intercalate - join lists with separator
intercalate(Sep, Lists) ->
    lists:join(Sep, Lists).

%% Zip two lists into list of tuples
zip(L1, L2) ->
    lists:zipwith(fun(A, B) -> {'_tuple', A, B} end, L1, L2).
