module Data.Char where

-- Character utilities
-- In Erlang, characters are integers (Unicode code points)

-- Check if alphabetic
isAlpha :: Char -> Boolean
isAlpha c = isAlphaImpl c

foreign import isAlphaImpl :: Char -> Boolean = "call 'erlang':'or'(call 'erlang':'and'(call 'erlang':'>='($0, 65), call 'erlang':'=<'($0, 90)), call 'erlang':'and'(call 'erlang':'>='($0, 97), call 'erlang':'=<'($0, 122)))"

-- Check if digit
isDigit :: Char -> Boolean
isDigit c = isDigitImpl c

foreign import isDigitImpl :: Char -> Boolean = "call 'erlang':'and'(call 'erlang':'>='($0, 48), call 'erlang':'=<'($0, 57))"

-- Check if alphanumeric
isAlphaNum :: Char -> Boolean
isAlphaNum c = isAlphaNumImpl c

foreign import isAlphaNumImpl :: Char -> Boolean = "call 'erlang':'or'(call 'erlang':'or'(call 'erlang':'and'(call 'erlang':'>='($0, 65), call 'erlang':'=<'($0, 90)), call 'erlang':'and'(call 'erlang':'>='($0, 97), call 'erlang':'=<'($0, 122))), call 'erlang':'and'(call 'erlang':'>='($0, 48), call 'erlang':'=<'($0, 57)))"

-- Check if whitespace
isSpace :: Char -> Boolean
isSpace c = isSpaceImpl c

foreign import isSpaceImpl :: Char -> Boolean = "call 'erlang':'or'(call 'erlang':'or'(call 'erlang':'=='($0, 32), call 'erlang':'=='($0, 9)), call 'erlang':'or'(call 'erlang':'=='($0, 10), call 'erlang':'or'(call 'erlang':'=='($0, 13), call 'erlang':'=='($0, 12))))"

-- Check if uppercase
isUpper :: Char -> Boolean
isUpper c = isUpperImpl c

foreign import isUpperImpl :: Char -> Boolean = "call 'erlang':'and'(call 'erlang':'>='($0, 65), call 'erlang':'=<'($0, 90))"

-- Check if lowercase
isLower :: Char -> Boolean
isLower c = isLowerImpl c

foreign import isLowerImpl :: Char -> Boolean = "call 'erlang':'and'(call 'erlang':'>='($0, 97), call 'erlang':'=<'($0, 122))"

-- Convert to uppercase
toUpper :: Char -> Char
toUpper c = toUpperImpl c

foreign import toUpperImpl :: Char -> Char = "case call 'erlang':'and'(call 'erlang':'>='($0, 97), call 'erlang':'=<'($0, 122)) of\n        <'true'> when 'true' -> call 'erlang':'-'($0, 32)\n        <'false'> when 'true' -> $0\n      end"

-- Convert to lowercase
toLower :: Char -> Char
toLower c = toLowerImpl c

foreign import toLowerImpl :: Char -> Char = "case call 'erlang':'and'(call 'erlang':'>='($0, 65), call 'erlang':'=<'($0, 90)) of\n        <'true'> when 'true' -> call 'erlang':'+'($0, 32)\n        <'false'> when 'true' -> $0\n      end"

-- Get code point
toCharCode :: Char -> Int
toCharCode c = toCharCodeImpl c

foreign import toCharCodeImpl :: Char -> Int = "$0"

-- Create from code point
fromCharCode :: Int -> Char
fromCharCode n = fromCharCodeImpl n

foreign import fromCharCodeImpl :: Int -> Char = "$0"

-- Check if control character (0-31 or 127)
isControl :: Char -> Boolean
isControl c = isControlImpl c

foreign import isControlImpl :: Char -> Boolean = "call 'erlang':'or'(call 'erlang':'=<'($0, 31), call 'erlang':'=='($0, 127))"

-- Check if printable (32-126)
isPrint :: Char -> Boolean
isPrint c = isPrintImpl c

foreign import isPrintImpl :: Char -> Boolean = "call 'erlang':'and'(call 'erlang':'>='($0, 32), call 'erlang':'=<'($0, 126))"

-- Check if hexadecimal digit (0-9, A-F, a-f)
isHexDigit :: Char -> Boolean
isHexDigit c = isHexDigitImpl c

foreign import isHexDigitImpl :: Char -> Boolean = "call 'erlang':'or'(call 'erlang':'or'(call 'erlang':'and'(call 'erlang':'>='($0, 48), call 'erlang':'=<'($0, 57)), call 'erlang':'and'(call 'erlang':'>='($0, 65), call 'erlang':'=<'($0, 70))), call 'erlang':'and'(call 'erlang':'>='($0, 97), call 'erlang':'=<'($0, 102)))"

-- Check if octal digit (0-7)
isOctDigit :: Char -> Boolean
isOctDigit c = isOctDigitImpl c

foreign import isOctDigitImpl :: Char -> Boolean = "call 'erlang':'and'(call 'erlang':'>='($0, 48), call 'erlang':'=<'($0, 55))"

-- Check if ASCII (0-127)
isAscii :: Char -> Boolean
isAscii c = isAsciiImpl c

foreign import isAsciiImpl :: Char -> Boolean = "call 'erlang':'and'(call 'erlang':'>='($0, 0), call 'erlang':'=<'($0, 127))"

-- Check if Latin-1 (first 256 code points)
isLatin1 :: Char -> Boolean
isLatin1 c = isLatin1Impl c

foreign import isLatin1Impl :: Char -> Boolean = "call 'erlang':'and'(call 'erlang':'>='($0, 0), call 'erlang':'=<'($0, 255))"
