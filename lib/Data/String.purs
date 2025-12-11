module Data.String where

import Data.Maybe (Maybe)

-- Pattern and Replacement newtypes for API compatibility with PureScript
newtype Pattern = Pattern String
newtype Replacement = Replacement String

-- String length
length :: String -> Int
length s = lengthImpl s

foreign import lengthImpl :: String -> Int

-- Take first n characters
take :: Int -> String -> String
take n s = takeImpl n s

foreign import takeImpl :: Int -> String -> String

-- Drop first n characters
drop :: Int -> String -> String
drop n s = dropImpl n s

foreign import dropImpl :: Int -> String -> String

-- Character at index
charAt :: Int -> String -> Maybe Char
charAt n s = charAtImpl n s

foreign import charAtImpl :: Int -> String -> Maybe Char

-- Check if string contains pattern
contains :: String -> String -> Boolean
contains pattern s = containsImpl pattern s

foreign import containsImpl :: String -> String -> Boolean

-- Split string by separator (Pattern version for PureScript API compatibility)
split :: Pattern -> String -> Array String
split (Pattern sep) s = splitImpl sep s

foreign import splitImpl :: String -> String -> Array String

-- Join strings with separator
joinWith :: String -> Array String -> String
joinWith sep xs = joinWithImpl sep xs

foreign import joinWithImpl :: String -> Array String -> String

-- Replace all occurrences (Pattern/Replacement version for PureScript API compatibility)
replaceAll :: Pattern -> Replacement -> String -> String
replaceAll (Pattern pat) (Replacement rep) s = replaceAllImpl pat rep s

foreign import replaceAllImpl :: String -> String -> String -> String

-- Trim whitespace
trim :: String -> String
trim s = trimImpl s

foreign import trimImpl :: String -> String

-- To lowercase
toLower :: String -> String
toLower s = toLowerImpl s

foreign import toLowerImpl :: String -> String

-- To uppercase
toUpper :: String -> String
toUpper s = toUpperImpl s

foreign import toUpperImpl :: String -> String

-- Strip prefix
stripPrefix :: String -> String -> Maybe String
stripPrefix prefix s = stripPrefixImpl prefix s

foreign import stripPrefixImpl :: String -> String -> Maybe String

-- Last index of pattern (Pattern version for PureScript API compatibility)
lastIndexOf :: Pattern -> String -> Maybe Int
lastIndexOf (Pattern pat) s = lastIndexOfImpl pat s

foreign import lastIndexOfImpl :: String -> String -> Maybe Int

-- Convert string to integer
toInt :: String -> Maybe Int
toInt s = toIntImpl s

foreign import toIntImpl :: String -> Maybe Int

-- Convert string to character array (code points)
toCodePointArray :: String -> Array Int
toCodePointArray s = toCodePointArrayImpl s

foreign import toCodePointArrayImpl :: String -> Array Int

-- Create string from single code point
singleton :: Int -> String
singleton cp = singletonImpl cp

foreign import singletonImpl :: Int -> String

-- Create string from character array
fromCharArray :: Array Char -> String
fromCharArray cs = fromCharArrayImpl cs

foreign import fromCharArrayImpl :: Array Char -> String

-- String.CodeUnits (CU) functions

-- Index into string (by code unit)
codeUnitIndex :: Int -> String -> Maybe Char
codeUnitIndex i s = codeUnitIndexImpl i s

foreign import codeUnitIndexImpl :: Int -> String -> Maybe Char

-- Take n code units
codeUnitTake :: Int -> String -> String
codeUnitTake n s = codeUnitTakeImpl n s

foreign import codeUnitTakeImpl :: Int -> String -> String

-- Drop n code units
codeUnitDrop :: Int -> String -> String
codeUnitDrop n s = codeUnitDropImpl n s

foreign import codeUnitDropImpl :: Int -> String -> String

-- Length in code units
codeUnitLength :: String -> Int
codeUnitLength s = codeUnitLengthImpl s

foreign import codeUnitLengthImpl :: String -> Int

-- Index of substring (Pattern version for PureScript API compatibility)
indexOf :: Pattern -> String -> Maybe Int
indexOf (Pattern pat) s = indexOfImpl pat s

foreign import indexOfImpl :: String -> String -> Maybe Int

-- Uncons (split first char from rest)
uncons :: String -> Maybe { head :: Char, tail :: String }
uncons s = unconsImpl s

foreign import unconsImpl :: String -> Maybe { head :: Char, tail :: String }

-- String.CodePoints (SCU) functions

-- Slice substring
slice :: Int -> Int -> String -> String
slice start end s = sliceImpl start end s

foreign import sliceImpl :: Int -> Int -> String -> String

-- Null check
null :: String -> Boolean
null s = nullImpl s

foreign import nullImpl :: String -> Boolean

-- Code unit at index (returns Int instead of Char)
codePointAt :: Int -> String -> Maybe Int
codePointAt i s = codePointAtImpl i s

foreign import codePointAtImpl :: Int -> String -> Maybe Int

-- From code point array
fromCodePointArray :: Array Int -> String
fromCodePointArray cps = fromCodePointArrayImpl cps

foreign import fromCodePointArrayImpl :: Array Int -> String
