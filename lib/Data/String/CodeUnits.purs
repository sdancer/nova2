module Data.String.CodeUnits where

import Data.Maybe (Maybe)

-- | Create a string from a single character
singleton :: Char -> String
singleton c = singletonImpl c

foreign import singletonImpl :: Char -> String = "[$0]"

-- | Take a number of code units from the start of a string
take :: Int -> String -> String
take n s = takeImpl n s

foreign import takeImpl :: Int -> String -> String = "call 'lists':'sublist'($1, $0)"

-- | Drop a number of code units from the start of a string
drop :: Int -> String -> String
drop n s = dropImpl n s

foreign import dropImpl :: Int -> String -> String = "case $0 of\n        <0> when 'true' -> $1\n        <N> when 'true' -> call 'lists':'nthtail'(N, $1)\n      end"

-- | Get the length in code units
length :: String -> Int
length s = lengthImpl s

foreign import lengthImpl :: String -> Int = "call 'erlang':'length'($0)"

-- | Get the character at a specific index
charAt :: Int -> String -> Maybe Char
charAt i s = charAtImpl i s

foreign import charAtImpl :: Int -> String -> Maybe Char = "case call 'erlang':'and'(call 'erlang':'>='($0, 0), call 'erlang':'<'($0, call 'erlang':'length'($1))) of\n        <'false'> when 'true' -> 'Nothing'\n        <'true'> when 'true' -> {'Just', call 'lists':'nth'(call 'erlang':'+'($0, 1), $1)}\n      end"

-- | Get the code unit value at a specific index
uncons :: String -> Maybe { head :: Char, tail :: String }
uncons s = unconsImpl s

foreign import unconsImpl :: String -> Maybe { head :: Char, tail :: String } = "case $0 of\n        <[]> when 'true' -> 'Nothing'\n        <[H|T]> when 'true' -> {'Just', {'head', H, 'tail', T}}\n      end"

-- | Convert string to array of characters
toCharArray :: String -> Array Char
toCharArray s = toCharArrayImpl s

foreign import toCharArrayImpl :: String -> Array Char = "$0"

-- | Convert array of characters to string
fromCharArray :: Array Char -> String
fromCharArray cs = fromCharArrayImpl cs

foreign import fromCharArrayImpl :: Array Char -> String = "$0"
