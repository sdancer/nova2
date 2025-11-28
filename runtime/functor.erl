-module(functor).
-export([map/2, foldl/3, foldr/3]).

%% Polymorphic map function that works on both lists and maps
map(F, Collection) when is_list(Collection) ->
    lists:map(F, Collection);
map(F, Collection) when is_map(Collection) ->
    maps:map(fun(_K, V) -> F(V) end, Collection).

%% Polymorphic foldl for lists and maps (maps fold over values like PureScript)
%% F can be either curried (1-arity) or uncurried (2-arity)
foldl(F, Acc, Collection) when is_list(Collection) ->
    lists:foldl(fun(Elem, A) ->
        case erlang:fun_info(F, arity) of
            {arity, 1} -> (F(A))(Elem);  % curried
            {arity, 2} -> F(A, Elem)      % uncurried
        end
    end, Acc, Collection);
foldl(F, Acc, Collection) when is_map(Collection) ->
    %% For maps, fold over values only (like PureScript Foldable Map)
    maps:fold(fun(_K, V, A) ->
        case erlang:fun_info(F, arity) of
            {arity, 1} -> (F(A))(V);  % curried
            {arity, 2} -> F(A, V)      % uncurried
        end
    end, Acc, Collection).

%% Polymorphic foldr for lists and maps
foldr(F, Acc, Collection) when is_list(Collection) ->
    lists:foldr(fun(Elem, A) ->
        case erlang:fun_info(F, arity) of
            {arity, 1} -> (F(Elem))(A);  % curried
            {arity, 2} -> F(Elem, A)     % uncurried
        end
    end, Acc, Collection);
foldr(F, Acc, Collection) when is_map(Collection) ->
    %% For maps, fold over values only (convert to list first for foldr)
    Values = maps:values(Collection),
    lists:foldr(fun(V, A) ->
        case erlang:fun_info(F, arity) of
            {arity, 1} -> (F(V))(A);  % curried
            {arity, 2} -> F(V, A)     % uncurried
        end
    end, Acc, Values).
