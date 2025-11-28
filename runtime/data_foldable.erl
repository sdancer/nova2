-module(data_foldable).
-export([foldM/3]).

%% Monadic fold (for Either monad)
%% foldM :: (acc -> elem -> Either err acc) -> acc -> [elem] -> Either err acc
foldM(F, Init, List) ->
    foldM_impl(F, {'_right', Init}, List).

foldM_impl(_F, {'_left', Err}, _List) ->
    {'_left', Err};
foldM_impl(_F, {'_right', Acc}, []) ->
    {'_right', Acc};
foldM_impl(F, {'_right', Acc}, [H|T]) ->
    case F(Acc, H) of
        {'_left', Err} -> {'_left', Err};
        {'_right', NewAcc} -> foldM_impl(F, {'_right', NewAcc}, T)
    end.
