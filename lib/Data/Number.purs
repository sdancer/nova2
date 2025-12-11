module Data.Number where

import Data.Maybe (Maybe(..))

-- Parse Number from String
fromString :: String -> Maybe Number
fromString s = fromStringImpl s

foreign import fromStringImpl :: String -> Maybe Number = "case call 'string':'to_float'($0) of\n        <{F, []}> when 'true' -> {'Just', F}\n        <_> when 'true' -> case call 'string':'to_integer'($0) of\n          <{I, []}> when 'true' -> {'Just', call 'erlang':'float'(I)}\n          <_> when 'true' -> 'Nothing'\n        end\n      end"

-- Check if NaN
isNaN :: Number -> Boolean
isNaN n = isNaNImpl n

foreign import isNaNImpl :: Number -> Boolean = "call 'erlang':'is_float'($0) andalso call 'erlang':'not'(call 'erlang':'=='($0, $0))"

-- Check if finite
isFinite :: Number -> Boolean
isFinite n = isFiniteImpl n

foreign import isFiniteImpl :: Number -> Boolean = "call 'erlang':'and'(call 'erlang':'=='($0, $0), call 'erlang':'and'(call 'erlang':'/='($0, call 'math':'exp'(710.0)), call 'erlang':'/='($0, call 'erlang':'-'(0.0, call 'math':'exp'(710.0)))))"
