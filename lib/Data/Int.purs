module Data.Int where

import Data.Maybe (Maybe(..))

-- Parse Int from String
fromString :: String -> Maybe Int
fromString s = fromStringImpl s

foreign import fromStringImpl :: String -> Maybe Int = "case call 'string':'to_integer'($0) of\n        <{I, []}> when 'true' -> {'Just', I}\n        <_> when 'true' -> 'Nothing'\n      end"

-- Convert to Number
toNumber :: Int -> Number
toNumber i = toNumberImpl i

foreign import toNumberImpl :: Int -> Number = "call 'erlang':'float'($0)"

-- Round Number to Int
round :: Number -> Int
round n = roundImpl n

foreign import roundImpl :: Number -> Int = "call 'erlang':'round'($0)"

-- Floor Number to Int
floor :: Number -> Int
floor n = floorImpl n

foreign import floorImpl :: Number -> Int = "call 'erlang':'floor'($0)"

-- Ceiling Number to Int
ceil :: Number -> Int
ceil n = ceilImpl n

foreign import ceilImpl :: Number -> Int = "call 'erlang':'ceil'($0)"
