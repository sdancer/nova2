-module(array).
-export([all/2, any/2, cons/2, drop/2, dropWhile/2, elem/2, filter/1, filter/2, find/2,
         findIndex/2, foldl/3, foldr/3, head/1, index/2, init/1, last/1,
         length/1, nub/1, nubByEq/2, null/1, partition/2, range/2, replicate/2,
         reverse/1, snoc/2, span/2, tail/1, take/2, takeWhile/2, uncons/1,
         zip/2, mapMaybe/2, concatMap/2]).

%% Check if all elements satisfy predicate
all(F, List) -> lists:all(F, List).

%% Check if any element satisfies predicate
any(F, List) -> lists:any(F, List).

%% Cons element to front of list
cons(X, Xs) -> [X | Xs].

%% Drop first N elements
drop(N, List) -> lists:nthtail(min(N, erlang:length(List)), List).

%% Drop elements while predicate is true
dropWhile(F, List) -> lists:dropwhile(F, List).

%% Check if element is in list
elem(X, List) -> lists:member(X, List).

%% Filter list by predicate (curried)
filter(F) -> fun(List) -> lists:filter(F, List) end.
filter(F, List) -> lists:filter(F, List).

%% Find first element satisfying predicate
find(F, List) ->
    case lists:dropwhile(fun(X) -> not F(X) end, List) of
        [] -> '_nothing';
        [H|_] -> {'_just', H}
    end.

%% Find index of first element satisfying predicate
findIndex(F, List) -> findIndex(F, List, 0).
findIndex(_F, [], _I) -> '_nothing';
findIndex(F, [H|T], I) ->
    case F(H) of
        true -> {'_just', I};
        false -> findIndex(F, T, I + 1)
    end.

%% Get element at index (returns Maybe)
index(List, I) when I < 0 -> '_nothing';
index(List, I) ->
    case I < erlang:length(List) of
        true -> {'_just', lists:nth(I + 1, List)};
        false -> '_nothing'
    end.

%% Fold left - F can be either:
%%   - Curried: F(Acc) returns fun(Elem) -> NewAcc
%%   - Uncurried: F(Acc, Elem) -> NewAcc
foldl(F, Acc, List) ->
    lists:foldl(fun(Elem, A) ->
        case erlang:fun_info(F, arity) of
            {arity, 1} -> (F(A))(Elem);  % curried
            {arity, 2} -> F(A, Elem)      % uncurried
        end
    end, Acc, List).

%% Fold right - F can be either curried or uncurried
foldr(F, Acc, List) ->
    lists:foldr(fun(Elem, A) ->
        case erlang:fun_info(F, arity) of
            {arity, 1} -> (F(Elem))(A);  % curried
            {arity, 2} -> F(Elem, A)     % uncurried
        end
    end, Acc, List).

%% Get first element (returns Maybe)
head([]) -> '_nothing';
head([H|_]) -> {'_just', H}.

%% Get all but last element
init([]) -> [];
init([_]) -> [];
init([H|T]) -> [H | init(T)].

%% Get last element (returns Maybe)
last([]) -> '_nothing';
last([X]) -> {'_just', X};
last([_|T]) -> last(T).

%% Get length
length(List) -> erlang:length(List).

%% Remove duplicates (using equality)
nub(List) -> nub(List, []).
nub([], Acc) -> lists:reverse(Acc);
nub([H|T], Acc) ->
    case lists:member(H, Acc) of
        true -> nub(T, Acc);
        false -> nub(T, [H|Acc])
    end.

%% Remove duplicates using custom equality function (2-arity uncurried)
nubByEq(Eq, List) -> nubByEq(Eq, List, []).
nubByEq(_Eq, [], Acc) -> lists:reverse(Acc);
nubByEq(Eq, [H|T], Acc) ->
    case lists:any(fun(X) -> Eq(H, X) end, Acc) of
        true -> nubByEq(Eq, T, Acc);
        false -> nubByEq(Eq, T, [H|Acc])
    end.

%% Check if empty
null([]) -> true;
null(_) -> false.

%% Partition by predicate
partition(F, List) ->
    {Yes, No} = lists:partition(F, List),
    #{'yes' => Yes, 'no' => No}.

%% Generate range [From..To]
range(From, To) when From > To -> [];
range(From, To) -> lists:seq(From, To).

%% Replicate value N times
replicate(N, X) -> lists:duplicate(N, X).

%% Reverse list
reverse(List) -> lists:reverse(List).

%% Append element to end
snoc(List, X) -> List ++ [X].

%% Split list by predicate (span)
span(F, List) ->
    {Init, Rest} = lists:splitwith(F, List),
    #{init => Init, rest => Rest}.

%% Get tail
tail([]) -> [];
tail([_|T]) -> T.

%% Take first N elements
take(N, List) -> lists:sublist(List, N).

%% Take elements while predicate is true
takeWhile(F, List) -> lists:takewhile(F, List).

%% Uncons - split into head and tail
uncons([]) -> '_nothing';
uncons([H|T]) -> {'_just', #{'head' => H, 'tail' => T}}.

%% Zip two lists
zip(L1, L2) ->
    lists:zipwith(fun(A, B) -> {'_tuple', A, B} end, L1, L2).

%% Map and filter out Nothing values
mapMaybe(F, List) ->
    lists:filtermap(fun(X) ->
        case F(X) of
            '_nothing' -> false;
            {'_just', V} -> {true, V}
        end
    end, List).

%% ConcatMap (flatMap)
concatMap(F, List) ->
    lists:flatmap(F, List).
