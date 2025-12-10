module Data.String.CodeUnits where

import Data.Maybe (Maybe)

-- | Create a string from a single character
singleton :: Char -> String
singleton c = singletonImpl c

foreign import singletonImpl :: Char -> String

-- | Take a number of code units from the start of a string
take :: Int -> String -> String
take n s = takeImpl n s

foreign import takeImpl :: Int -> String -> String

-- | Drop a number of code units from the start of a string
drop :: Int -> String -> String
drop n s = dropImpl n s

foreign import dropImpl :: Int -> String -> String

-- | Get the length in code units
length :: String -> Int
length s = lengthImpl s

foreign import lengthImpl :: String -> Int

-- | Get the character at a specific index
charAt :: Int -> String -> Maybe Char
charAt i s = charAtImpl i s

foreign import charAtImpl :: Int -> String -> Maybe Char

-- | Get the code unit value at a specific index
uncons :: String -> Maybe { head :: Char, tail :: String }
uncons s = unconsImpl s

foreign import unconsImpl :: String -> Maybe { head :: Char, tail :: String }

-- | Convert string to array of characters
toCharArray :: String -> Array Char
toCharArray s = toCharArrayImpl s

foreign import toCharArrayImpl :: String -> Array Char

-- | Convert array of characters to string
fromCharArray :: Array Char -> String
fromCharArray cs = fromCharArrayImpl cs

foreign import fromCharArrayImpl :: Array Char -> String
