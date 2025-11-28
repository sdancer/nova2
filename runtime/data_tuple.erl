-module(data_tuple).
-export([fst/1, snd/1, 'Tuple'/2]).

%% Get first element of tuple
fst({'_tuple', A, _}) -> A.

%% Get second element of tuple
snd({'_tuple', _, B}) -> B.

%% Create a tuple
'Tuple'(A, B) -> {'_tuple', A, B}.
