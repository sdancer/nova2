-- | StringBuilder - efficient string building using iodata
-- |
-- | Instead of concatenating strings (O(n²) copying), we build
-- | a nested list structure that gets flattened once at the end.
-- | This is the standard BEAM pattern for efficient string building.
module Data.StringBuilder where

import Prelude

-- | A Builder is an opaque wrapper around iodata
-- | Internally it's [binary | [binary | ...]] but we hide that
foreign import data Builder :: Type

-- | Empty builder
empty :: Builder
empty = emptyImpl

foreign import emptyImpl :: Builder
  = "[]"

-- | Create a builder from a single string
singleton :: String -> Builder
singleton s = singletonImpl s

foreign import singletonImpl :: String -> Builder
  = "[$0]"

-- | Append two builders (O(1) - just cons)
append :: Builder -> Builder -> Builder
append a b = appendImpl a b

foreign import appendImpl :: Builder -> Builder -> Builder
  = "[$0 | $1]"

-- | Append a string to a builder
appendStr :: Builder -> String -> Builder
appendStr b s = appendStrImpl b s

foreign import appendStrImpl :: Builder -> String -> Builder
  = "[$0, $1]"

-- | Prepend a string to a builder
prependStr :: String -> Builder -> Builder
prependStr s b = prependStrImpl s b

foreign import prependStrImpl :: String -> Builder -> Builder
  = "[$0 | $1]"

-- | Build the final string (O(n) - single pass flatten)
build :: Builder -> String
build b = buildImpl b

foreign import buildImpl :: Builder -> String
  = "call 'erlang':'iolist_to_binary'($0)"

-- | Concatenate an array of strings efficiently
concat :: Array String -> String
concat arr = build (concatBuilder arr)

-- | Build from array of strings
concatBuilder :: Array String -> Builder
concatBuilder arr = concatBuilderImpl arr

foreign import concatBuilderImpl :: Array String -> Builder
  = "$0"

-- | Join strings with a separator
joinWith :: String -> Array String -> String
joinWith sep arr = joinWithImpl sep arr

foreign import joinWithImpl :: String -> Array String -> String
  = "call 'erlang':'iolist_to_binary'(call 'lists':'join'($0, $1))"

-- | Semigroup instance via append
infixr 5 append as <>>

-- | Build string from array of strings (most efficient - single iolist_to_binary)
-- | Usage: str ["call ", modName, ":", funcName, "(", args, ")"]
str :: Array String -> String
str parts = strImpl parts

foreign import strImpl :: Array String -> String
  = "call 'erlang':'iolist_to_binary'($0)"

-- | Intersperse separator between strings then build
-- | Like joinWith but takes array directly
intersperse :: String -> Array String -> String
intersperse sep parts = intersperseImpl sep parts

foreign import intersperseImpl :: String -> Array String -> String
  = "call 'erlang':'iolist_to_binary'(call 'lists':'join'($0, $1))"
