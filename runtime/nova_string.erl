-module(nova_string).
-export(['Pattern'/1, 'Replacement'/1, contains/2, drop/2, indexOf/2, joinWith/2, lastIndexOf/2, length/1, replaceAll/3, singleton/1, split/2, take/2, toCodePointArray/1, toLower/1, toUpper/1]).

%% Pattern newtype constructor (identity for strings)
'Pattern'(S) -> S.

%% Replacement newtype constructor (identity for strings)
'Replacement'(S) -> S.

%% Replace all occurrences of Pattern with Replacement in string
replaceAll(Pattern, Replacement, Str) ->
    binary_to_list(binary:replace(list_to_binary(Str), list_to_binary(Pattern), list_to_binary(Replacement), [global])).

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

%% Find first index of pattern in string
indexOf(Pattern, Str) ->
    case binary:match(list_to_binary(Str), list_to_binary(Pattern)) of
        nomatch -> '_nothing';
        {Pos, _Len} -> {'_just', Pos}
    end.

%% Find last index of pattern in string
lastIndexOf(Pattern, Str) ->
    Bin = list_to_binary(Str),
    PatBin = list_to_binary(Pattern),
    case binary:matches(Bin, PatBin) of
        [] -> '_nothing';
        Matches ->
            {LastPos, _Len} = lists:last(Matches),
            {'_just', LastPos}
    end.

%% Join strings with separator
joinWith(_Sep, []) -> [];
joinWith(_Sep, [X]) -> X;
joinWith(Sep, [H|T]) -> H ++ Sep ++ joinWith(Sep, T).

%% Convert string to array of code points (characters)
toCodePointArray(Str) -> Str.  % Erlang strings are already lists of code points

%% Convert string to lowercase
toLower(Str) -> string:lowercase(Str).

%% Convert string to uppercase
toUpper(Str) -> string:uppercase(Str).
