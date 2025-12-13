module Data.String.CodeUnits where

import Data.Maybe (Maybe)

-- | Create a string from a single character (code point)
singleton :: Char -> String
singleton c = singletonImpl c

foreign import singletonImpl :: Char -> String = "call 'unicode':'characters_to_binary'([$0])"

-- | Take a number of code units (bytes) from the start of a string
take :: Int -> String -> String
take n s = takeImpl n s

foreign import takeImpl :: Int -> String -> String = "let <Len> = call 'erlang':'byte_size'($1) in let <N> = case call 'erlang':'>'($0, Len) of <'true'> when 'true' -> Len <'false'> when 'true' -> $0 end in call 'erlang':'binary_part'($1, 0, N)"

-- | Drop a number of code units (bytes) from the start of a string
drop :: Int -> String -> String
drop n s = dropImpl n s

foreign import dropImpl :: Int -> String -> String = "let <Len> = call 'erlang':'byte_size'($1) in case call 'erlang':'>='($0, Len) of <'true'> when 'true' -> #{}# <'false'> when 'true' -> call 'erlang':'binary_part'($1, $0, call 'erlang':'-'(Len, $0)) end"

-- | Get the length in code units (bytes)
length :: String -> Int
length s = lengthImpl s

foreign import lengthImpl :: String -> Int = "call 'erlang':'byte_size'($0)"

-- | Get the character at a specific index (byte position)
charAt :: Int -> String -> Maybe Char
charAt i s = charAtImpl i s

foreign import charAtImpl :: Int -> String -> Maybe Char = "let <Len> = call 'erlang':'byte_size'($1) in case call 'erlang':'and'(call 'erlang':'>='($0, 0), call 'erlang':'<'($0, Len)) of <'false'> when 'true' -> 'Nothing' <'true'> when 'true' -> {'Just', call 'erlang':'binary_part'($1, $0, 1)} end"

-- | Uncons - split first byte from rest
uncons :: String -> Maybe { head :: Char, tail :: String }
uncons s = unconsImpl s

foreign import unconsImpl :: String -> Maybe { head :: Char, tail :: String } = "case call 'erlang':'byte_size'($0) of <0> when 'true' -> 'Nothing' <_> when 'true' -> let <H> = call 'erlang':'binary_part'($0, 0, 1) in let <T> = call 'erlang':'binary_part'($0, 1, call 'erlang':'-'(call 'erlang':'byte_size'($0), 1)) in {'Just', ~{'head'=>H,'tail'=>T}~} end"

-- | Convert string to array of characters (code points)
toCharArray :: String -> Array Char
toCharArray s = toCharArrayImpl s

foreign import toCharArrayImpl :: String -> Array Char = "call 'unicode':'characters_to_list'($0)"

-- | Convert array of characters to string
fromCharArray :: Array Char -> String
fromCharArray cs = fromCharArrayImpl cs

foreign import fromCharArrayImpl :: Array Char -> String = "call 'unicode':'characters_to_binary'($0)"
