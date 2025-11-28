-module(scu).
-export([charAt/2, singleton/1, length/1, take/2, drop/2, slice/3, uncons/1]).

%% Get character at position (returns Maybe)
charAt(Index, Str) when Index >= 0, Index < erlang:length(Str) ->
    {'_just', lists:nth(Index + 1, Str)};
charAt(_Index, _Str) ->
    '_nothing'.

%% Create a single-character string from a code point
singleton(CodePoint) -> [CodePoint].

%% Get string length in code units
length(Str) -> erlang:length(Str).

%% Take first N characters
take(N, Str) -> lists:sublist(Str, N).

%% Drop first N characters
drop(N, Str) when N >= erlang:length(Str) -> [];
drop(N, Str) -> lists:nthtail(N, Str).

%% Slice string from Start to End
slice(Start, End, Str) ->
    lists:sublist(lists:nthtail(Start, Str), End - Start).

%% Uncons - split head and tail
uncons([]) -> '_nothing';
uncons([H|T]) -> {'_just', #{head => H, tail => T}}.
