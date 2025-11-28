-module(int).
-export([fromString/1]).

%% Parse string to integer (returns Maybe)
fromString(Str) ->
    try
        {'_just', list_to_integer(Str)}
    catch
        error:badarg -> '_nothing'
    end.
