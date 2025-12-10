module Data.Number where

import Data.Maybe (Maybe(..))

-- Parse Number from String
fromString :: String -> Maybe Number
fromString s = fromStringImpl s

foreign import fromStringImpl :: String -> Maybe Number

-- Check if NaN
isNaN :: Number -> Boolean
isNaN n = isNaNImpl n

foreign import isNaNImpl :: Number -> Boolean

-- Check if finite
isFinite :: Number -> Boolean
isFinite n = isFiniteImpl n

foreign import isFiniteImpl :: Number -> Boolean
