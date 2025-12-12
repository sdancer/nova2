module Data.String where

import Data.Maybe (Maybe)

-- Pattern and Replacement newtypes for API compatibility with PureScript
newtype Pattern = Pattern String
newtype Replacement = Replacement String

-- String length
length :: String -> Int
length s = lengthImpl s

foreign import lengthImpl :: String -> Int = "call 'erlang':'length'($0)"

-- Take first n characters
take :: Int -> String -> String
take n s = takeImpl n s

foreign import takeImpl :: Int -> String -> String = "call 'lists':'sublist'($1, $0)"

-- Drop first n characters
drop :: Int -> String -> String
drop n s = dropImpl n s

foreign import dropImpl :: Int -> String -> String = "case $0 of\n        <0> when 'true' -> $1\n        <N> when 'true' -> call 'lists':'nthtail'(N, $1)\n      end"

-- Character at index
charAt :: Int -> String -> Maybe Char
charAt n s = charAtImpl n s

foreign import charAtImpl :: Int -> String -> Maybe Char = "case call 'erlang':'and'(call 'erlang':'>='($0, 0), call 'erlang':'<'($0, call 'erlang':'length'($1))) of\n        <'false'> when 'true' -> 'Nothing'\n        <'true'> when 'true' -> {'Just', call 'lists':'nth'(call 'erlang':'+'($0, 1), $1)}\n      end"

-- Check if string contains pattern
contains :: String -> String -> Boolean
contains pattern s = containsImpl pattern s

foreign import containsImpl :: String -> String -> Boolean = "case call 'string':'find'($1, $0) of\n        <'nomatch'> when 'true' -> 'false'\n        <_> when 'true' -> 'true'\n      end"

-- Split string by separator (Pattern version for PureScript API compatibility)
split :: Pattern -> String -> Array String
split (Pattern sep) s = splitImpl sep s

foreign import splitImpl :: String -> String -> Array String = "call 'string':'split'($1, $0, 'all')"

-- Join strings with separator
joinWith :: String -> Array String -> String
joinWith sep xs = joinWithImpl sep xs

foreign import joinWithImpl :: String -> Array String -> String = "call 'lists':'join'($0, $1)"

-- Replace all occurrences (Pattern/Replacement version for PureScript API compatibility)
replaceAll :: Pattern -> Replacement -> String -> String
replaceAll (Pattern pat) (Replacement rep) s = replaceAllImpl pat rep s

foreign import replaceAllImpl :: String -> String -> String -> String = "call 'string':'replace'($2, $0, $1, 'all')"

-- Trim whitespace
trim :: String -> String
trim s = trimImpl s

foreign import trimImpl :: String -> String = "call 'string':'trim'($0)"

-- To lowercase
toLower :: String -> String
toLower s = toLowerImpl s

foreign import toLowerImpl :: String -> String = "call 'string':'lowercase'($0)"

-- To uppercase
toUpper :: String -> String
toUpper s = toUpperImpl s

foreign import toUpperImpl :: String -> String = "call 'string':'uppercase'($0)"

-- Strip prefix
stripPrefix :: String -> String -> Maybe String
stripPrefix prefix s = stripPrefixImpl prefix s

foreign import stripPrefixImpl :: String -> String -> Maybe String = "case call 'lists':'prefix'($0, $1) of\n        <'true'> when 'true' -> {'Just', call 'lists':'nthtail'(call 'erlang':'length'($0), $1)}\n        <'false'> when 'true' -> 'Nothing'\n      end"

-- Last index of pattern (Pattern version for PureScript API compatibility)
lastIndexOf :: Pattern -> String -> Maybe Int
lastIndexOf (Pattern pat) s = lastIndexOfImpl pat s

foreign import lastIndexOfImpl :: String -> String -> Maybe Int = "case call 'string':'rstr'($1, $0) of\n        <0> when 'true' -> 'Nothing'\n        <N> when 'true' -> {'Just', call 'erlang':'-'(N, 1)}\n      end"

-- Convert string to integer
toInt :: String -> Maybe Int
toInt s = toIntImpl s

foreign import toIntImpl :: String -> Maybe Int = "case call 'string':'to_integer'($0) of\n        <{I, []}> when 'true' -> {'Just', I}\n        <_> when 'true' -> 'Nothing'\n      end"

-- Convert string to character array (code points)
toCodePointArray :: String -> Array Int
toCodePointArray s = toCodePointArrayImpl s

foreign import toCodePointArrayImpl :: String -> Array Int = "$0"

-- Create string from single code point
singleton :: Int -> String
singleton cp = singletonImpl cp

foreign import singletonImpl :: Int -> String = "[$0]"

-- Create string from character array
fromCharArray :: Array Char -> String
fromCharArray cs = fromCharArrayImpl cs

foreign import fromCharArrayImpl :: Array Char -> String = "$0"

-- String.CodeUnits (CU) functions

-- Index into string (by code unit)
codeUnitIndex :: Int -> String -> Maybe Char
codeUnitIndex i s = codeUnitIndexImpl i s

foreign import codeUnitIndexImpl :: Int -> String -> Maybe Char = "case call 'erlang':'and'(call 'erlang':'>='($0, 0), call 'erlang':'<'($0, call 'erlang':'length'($1))) of\n        <'false'> when 'true' -> 'Nothing'\n        <'true'> when 'true' -> {'Just', call 'lists':'nth'(call 'erlang':'+'($0, 1), $1)}\n      end"

-- Take n code units
codeUnitTake :: Int -> String -> String
codeUnitTake n s = codeUnitTakeImpl n s

foreign import codeUnitTakeImpl :: Int -> String -> String = "call 'lists':'sublist'($1, $0)"

-- Drop n code units
codeUnitDrop :: Int -> String -> String
codeUnitDrop n s = codeUnitDropImpl n s

foreign import codeUnitDropImpl :: Int -> String -> String = "case $0 of\n        <0> when 'true' -> $1\n        <N> when 'true' -> call 'lists':'nthtail'(N, $1)\n      end"

-- Length in code units
codeUnitLength :: String -> Int
codeUnitLength s = codeUnitLengthImpl s

foreign import codeUnitLengthImpl :: String -> Int = "call 'erlang':'length'($0)"

-- Index of substring (Pattern version for PureScript API compatibility)
indexOf :: Pattern -> String -> Maybe Int
indexOf (Pattern pat) s = indexOfImpl pat s

foreign import indexOfImpl :: String -> String -> Maybe Int = "case call 'string':'str'($1, $0) of\n        <0> when 'true' -> 'Nothing'\n        <N> when 'true' -> {'Just', call 'erlang':'-'(N, 1)}\n      end"

-- Uncons (split first char from rest)
uncons :: String -> Maybe { head :: Char, tail :: String }
uncons s = unconsImpl s

foreign import unconsImpl :: String -> Maybe { head :: Char, tail :: String } = "case $0 of\n        <[]> when 'true' -> 'Nothing'\n        <[H|T]> when 'true' -> {'Just', ~{'head'=>H,'tail'=>T}~}\n      end"

-- String.CodePoints (SCU) functions

-- Slice substring
slice :: Int -> Int -> String -> String
slice start end s = sliceImpl start end s

foreign import sliceImpl :: Int -> Int -> String -> String = "call 'lists':'sublist'(call 'lists':'nthtail'($0, $2), call 'erlang':'-'($1, $0))"

-- Null check
null :: String -> Boolean
null s = nullImpl s

foreign import nullImpl :: String -> Boolean = "case $0 of\n        <[]> when 'true' -> 'true'\n        <_> when 'true' -> 'false'\n      end"

-- Code unit at index (returns Int instead of Char)
codePointAt :: Int -> String -> Maybe Int
codePointAt i s = codePointAtImpl i s

foreign import codePointAtImpl :: Int -> String -> Maybe Int = "case call 'erlang':'and'(call 'erlang':'>='($0, 0), call 'erlang':'<'($0, call 'erlang':'length'($1))) of\n        <'false'> when 'true' -> 'Nothing'\n        <'true'> when 'true' -> {'Just', call 'lists':'nth'(call 'erlang':'+'($0, 1), $1)}\n      end"

-- From code point array
fromCodePointArray :: Array Int -> String
fromCodePointArray cps = fromCodePointArrayImpl cps

foreign import fromCodePointArrayImpl :: Array Int -> String = "$0"
