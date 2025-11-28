-module(scu).
-export([singleton/1]).

%% String CodeUnit utilities

%% Create single-character string from codepoint
singleton(Char) -> [Char].
