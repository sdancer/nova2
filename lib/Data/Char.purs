module Data.Char where

-- Character utilities

-- Check if alphabetic
isAlpha :: Char -> Boolean
isAlpha c = isAlphaImpl c

foreign import isAlphaImpl :: Char -> Boolean

-- Check if digit
isDigit :: Char -> Boolean
isDigit c = isDigitImpl c

foreign import isDigitImpl :: Char -> Boolean

-- Check if alphanumeric
isAlphaNum :: Char -> Boolean
isAlphaNum c = isAlphaNumImpl c

foreign import isAlphaNumImpl :: Char -> Boolean

-- Check if whitespace
isSpace :: Char -> Boolean
isSpace c = isSpaceImpl c

foreign import isSpaceImpl :: Char -> Boolean

-- Check if uppercase
isUpper :: Char -> Boolean
isUpper c = isUpperImpl c

foreign import isUpperImpl :: Char -> Boolean

-- Check if lowercase
isLower :: Char -> Boolean
isLower c = isLowerImpl c

foreign import isLowerImpl :: Char -> Boolean

-- Convert to uppercase
toUpper :: Char -> Char
toUpper c = toUpperImpl c

foreign import toUpperImpl :: Char -> Char

-- Convert to lowercase
toLower :: Char -> Char
toLower c = toLowerImpl c

foreign import toLowerImpl :: Char -> Char

-- Get code point
toCharCode :: Char -> Int
toCharCode c = toCharCodeImpl c

foreign import toCharCodeImpl :: Char -> Int

-- Create from code point
fromCharCode :: Int -> Char
fromCharCode n = fromCharCodeImpl n

foreign import fromCharCodeImpl :: Int -> Char

-- Check if control character
isControl :: Char -> Boolean
isControl c = isControlImpl c

foreign import isControlImpl :: Char -> Boolean

-- Check if printable
isPrint :: Char -> Boolean
isPrint c = isPrintImpl c

foreign import isPrintImpl :: Char -> Boolean

-- Check if hexadecimal digit
isHexDigit :: Char -> Boolean
isHexDigit c = isHexDigitImpl c

foreign import isHexDigitImpl :: Char -> Boolean

-- Check if octal digit
isOctDigit :: Char -> Boolean
isOctDigit c = isOctDigitImpl c

foreign import isOctDigitImpl :: Char -> Boolean

-- Check if ASCII
isAscii :: Char -> Boolean
isAscii c = isAsciiImpl c

foreign import isAsciiImpl :: Char -> Boolean

-- Check if Latin-1 (first 256 code points)
isLatin1 :: Char -> Boolean
isLatin1 c = isLatin1Impl c

foreign import isLatin1Impl :: Char -> Boolean
