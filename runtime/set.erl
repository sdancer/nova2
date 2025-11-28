-module(set).
-export([delete/2, difference/2, empty/0, insert/2, intersection/2,
         member/2, singleton/1, size/1, union/2, fromFoldable/1]).

%% Using Erlang's sets module internally

%% Empty set
empty() -> sets:new().

%% Insert element
insert(X, Set) -> sets:add_element(X, Set).

%% Delete element
delete(X, Set) -> sets:del_element(X, Set).

%% Check membership
member(X, Set) -> sets:is_element(X, Set).

%% Set size
size(Set) -> sets:size(Set).

%% Create singleton set
singleton(X) -> sets:from_list([X]).

%% Set union
union(S1, S2) -> sets:union(S1, S2).

%% Set intersection
intersection(S1, S2) -> sets:intersection(S1, S2).

%% Set difference
difference(S1, S2) -> sets:subtract(S1, S2).

%% Create set from list
fromFoldable(List) -> sets:from_list(List).
