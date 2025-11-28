-module(number).
-export([fromString/1]).

%% Parse string to number (returns Maybe)
fromString(Str) ->
    try
        {'_just', list_to_float(Str)}
    catch
        error:badarg ->
            try
                {'_just', float(list_to_integer(Str))}
            catch
                error:badarg -> '_nothing'
            end
    end.
