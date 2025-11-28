-module(nova_string).
-export(['Pattern'/1, contains/2, drop/2, joinWith/2, length/1, singleton/1, split/2, take/2]).

%% Pattern newtype constructor (identity for strings)
'Pattern'(S) -> S.

%% Check if string contains substring
contains(Substr, Str) ->
    case lists:search(fun(C) -> C =:= hd(Substr) end, Str) of
        false -> false;
        _ ->
            %% Simple substring search
            case binary:match(list_to_binary(Str), list_to_binary(Substr)) of
                nomatch -> false;
                _ -> true
            end
    end.

%% Drop first N characters
drop(N, Str) when N >= erlang:length(Str) -> [];
drop(N, Str) -> lists:nthtail(N, Str).

%% Get string length (in characters, not bytes)
length(Str) -> erlang:length(Str).

%% Create single-character string
singleton(Char) -> [Char].

%% Split string on pattern
split(_Pattern, []) -> [];
split(Pattern, Str) ->
    case binary:split(list_to_binary(Str), list_to_binary(Pattern), [global]) of
        Parts -> [binary_to_list(P) || P <- Parts]
    end.

%% Take first N characters
take(N, Str) -> lists:sublist(Str, N).

%% Join strings with separator
joinWith(_Sep, []) -> [];
joinWith(_Sep, [X]) -> X;
joinWith(Sep, [H|T]) -> H ++ Sep ++ joinWith(Sep, T).
