module Data.Int where

import Data.Maybe (Maybe(..))

-- Parse Int from String
fromString :: String -> Maybe Int
fromString s = fromStringImpl s

foreign import fromStringImpl :: String -> Maybe Int

-- Convert to Number
toNumber :: Int -> Number
toNumber i = toNumberImpl i

foreign import toNumberImpl :: Int -> Number

-- Round Number to Int
round :: Number -> Int
round n = roundImpl n

foreign import roundImpl :: Number -> Int

-- Floor Number to Int
floor :: Number -> Int
floor n = floorImpl n

foreign import floorImpl :: Number -> Int

-- Ceiling Number to Int
ceil :: Number -> Int
ceil n = ceilImpl n

foreign import ceilImpl :: Number -> Int
