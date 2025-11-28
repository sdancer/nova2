-module(cu).
-export([charAt/2, drop/2, length/1, singleton/1, take/2, toChar/1, uncons/1]).

%% Character utilities (CodeUnit operations)

%% Get character at index (returns Maybe)
charAt(Index, Str) when Index < 0 -> '_nothing';
charAt(Index, Str) ->
    case Index < erlang:length(Str) of
        true -> {'_just', lists:nth(Index + 1, Str)};
        false -> '_nothing'
    end.

%% Drop first N characters from string
drop(N, Str) -> string:slice(Str, N).

%% Get string length
length(Str) -> erlang:length(Str).

%% Create single-character string from codepoint
singleton(Char) -> [Char].

%% Take first N characters
take(N, Str) -> string:slice(Str, 0, N).

%% Convert single char string to char (returns Maybe)
toChar([C]) -> {'_just', C};
toChar(_) -> '_nothing'.

%% Uncons string (returns Maybe {head, tail})
uncons([]) -> '_nothing';
uncons([H|T]) -> {'_just', {H, T}}.
