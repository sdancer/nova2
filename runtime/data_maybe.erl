-module(data_maybe).
-export([fromMaybe/2]).

%% Extract value from Maybe with default
fromMaybe(Default, '_nothing') -> Default;
fromMaybe(_Default, {'_just', Value}) -> Value.
