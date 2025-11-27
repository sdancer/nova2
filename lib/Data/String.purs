module Data.String where

import Data.Maybe (Maybe)

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

-- Split string by separator
split :: String -> String -> Array String
split sep s = splitImpl sep s

foreign import splitImpl :: String -> String -> Array String

-- Join strings with separator
joinWith :: String -> Array String -> String
joinWith sep xs = joinWithImpl sep xs

foreign import joinWithImpl :: String -> Array String -> String

-- Replace all occurrences
replaceAll :: String -> String -> String -> String
replaceAll pattern replacement s = replaceAllImpl pattern replacement s

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

-- Last index of pattern
lastIndexOf :: String -> String -> Maybe Int
lastIndexOf pattern s = lastIndexOfImpl pattern s

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
