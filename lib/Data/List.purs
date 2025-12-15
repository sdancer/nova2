module Data.List where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- List operations
-- These are foreign imports that map to Erlang list operations

-- Convert from foldable (Array) to List
fromFoldable :: forall a. Array a -> List a
fromFoldable xs = fromFoldableImpl xs

foreign import fromFoldableImpl :: forall a. Array a -> List a = "$0"

-- Convert to unfoldable (Array)
toUnfoldable :: forall a. List a -> Array a
toUnfoldable xs = toUnfoldableImpl xs

foreign import toUnfoldableImpl :: forall a. List a -> Array a = "$0"

-- Check if list is null/empty
null :: forall a. List a -> Boolean
null xs = nullImpl xs

foreign import nullImpl :: forall a. List a -> Boolean = "case $0 of\n        <[]> when 'true' -> 'true'\n        <_> when 'true' -> 'false'\n      end"

-- Get head and tail as record
uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons xs = unconsImpl xs

foreign import unconsImpl :: forall a. List a -> Maybe { head :: a, tail :: List a } = "case $0 of\n        <[]> when 'true' -> 'Nothing'\n        <[H|T]> when 'true' -> {'Just', ~{'head'=>H,'tail'=>T}~}\n      end"

-- Get head (first element)
head :: forall a. List a -> Maybe a
head xs = headImpl xs

foreign import headImpl :: forall a. List a -> Maybe a = "case $0 of\n        <[]> when 'true' -> 'Nothing'\n        <[H|_]> when 'true' -> {'Just', H}\n      end"

-- Get tail (all but first)
tail :: forall a. List a -> Maybe (List a)
tail xs = tailImpl xs

foreign import tailImpl :: forall a. List a -> Maybe (List a) = "case $0 of\n        <[]> when 'true' -> 'Nothing'\n        <[_|T]> when 'true' -> {'Just', T}\n      end"

-- Reverse a list
reverse :: forall a. List a -> List a
reverse xs = reverseImpl xs

foreign import reverseImpl :: forall a. List a -> List a = "call 'lists':'reverse'($0)"

-- Length of a list
length :: forall a. List a -> Int
length xs = lengthImpl xs

foreign import lengthImpl :: forall a. List a -> Int = "call 'erlang':'length'($0)"

-- Prepend element (cons)
cons :: forall a. a -> List a -> List a
cons x xs = consImpl x xs

foreign import consImpl :: forall a. a -> List a -> List a = "[$0|$1]"

-- Infix cons operator
infixr 6 cons as :

-- Create singleton list
singleton :: forall a. a -> List a
singleton x = singletonImpl x

foreign import singletonImpl :: forall a. a -> List a = "[$0]"

-- Append two lists
append :: forall a. List a -> List a -> List a
append xs ys = appendImpl xs ys

foreign import appendImpl :: forall a. List a -> List a -> List a = "call 'lists':'append'($0, $1)"

-- Map over a list
map :: forall a b. (a -> b) -> List a -> List b
map f xs = mapImpl f xs

foreign import mapImpl :: forall a b. (a -> b) -> List a -> List b = "call 'lists':'map'($0, $1)"

-- Filter a list
filter :: forall a. (a -> Boolean) -> List a -> List a
filter f xs = filterImpl f xs

foreign import filterImpl :: forall a. (a -> Boolean) -> List a -> List a = "call 'lists':'filter'($0, $1)"

-- Fold right
foldr :: forall a b. (a -> b -> b) -> b -> List a -> b
foldr f acc xs = foldrImpl f acc xs

-- Note: Nova compiles multi-arg lambdas as multi-arity functions, so use uncurried apply
foreign import foldrImpl :: forall a b. (a -> b -> b) -> b -> List a -> b = "call 'lists':'foldr'(fun (E, A) -> apply $0 (E, A), $1, $2)"

-- Fold left - uses Erlang lists:foldl with uncurried function application
-- Note: We use uncurried apply (A, E) to match how partial applications are generated
-- IMPORTANT: When passing constructors to foldl, wrap them in a lambda to ensure
-- uncurried calling convention: foldl (\a b -> Con a b) instead of foldl Con
foldl :: forall a b. (b -> a -> b) -> b -> List a -> b
foldl f acc xs = foldlImpl f acc xs

foreign import foldlImpl :: forall a b. (b -> a -> b) -> b -> List a -> b = "call 'lists':'foldl'(fun (E, A) -> apply $0 (A, E), $1, $2)"

-- Take first n elements
take :: forall a. Int -> List a -> List a
take n xs = takeImpl n xs

foreign import takeImpl :: forall a. Int -> List a -> List a = "call 'lists':'sublist'($1, $0)"

-- Drop first n elements
drop :: forall a. Int -> List a -> List a
drop n xs = dropImpl n xs

foreign import dropImpl :: forall a. Int -> List a -> List a = "call 'lists':'nthtail'($0, $1)"

-- Any element satisfies predicate
any :: forall a. (a -> Boolean) -> List a -> Boolean
any f xs = anyImpl f xs

foreign import anyImpl :: forall a. (a -> Boolean) -> List a -> Boolean = "call 'lists':'any'($0, $1)"

-- Map with Maybe filter (keep only Just values)
mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
mapMaybe f xs = mapMaybeImpl f xs

foreign import mapMaybeImpl :: forall a b. (a -> Maybe b) -> List a -> List b = "call 'lists':'filtermap'(fun (X) -> case apply $0 (X) of\n        <'Nothing'> when 'true' -> 'false'\n        <{'Just', V}> when 'true' -> {'true', V}\n      end, $1)"

-- Take while predicate holds
takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile f xs = takeWhileImpl f xs

foreign import takeWhileImpl :: forall a. (a -> Boolean) -> List a -> List a = "call 'lists':'takewhile'($0, $1)"

-- Drop while predicate holds
dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile f xs = dropWhileImpl f xs

foreign import dropWhileImpl :: forall a. (a -> Boolean) -> List a -> List a = "call 'lists':'dropwhile'($0, $1)"

-- Create a range from start to end (inclusive)
range :: Int -> Int -> List Int
range start end = rangeImpl start end

foreign import rangeImpl :: Int -> Int -> List Int = "call 'lists':'seq'($0, $1)"

-- Check if element is in list
elem :: forall a. Eq a => a -> List a -> Boolean
elem x xs = elemImpl x xs

foreign import elemImpl :: forall a. a -> List a -> Boolean = "call 'lists':'member'($0, $1)"

-- Zip two lists together
zip :: forall a b. List a -> List b -> List (Tuple a b)
zip xs ys = zipImpl xs ys

foreign import zipImpl :: forall a b. List a -> List b -> List (Tuple a b) = "call 'lists':'zipwith'(fun (A, B) -> {'Tuple', A, B}, $0, $1)"

-- Zip with a function
zipWith :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = zipWithImpl f xs ys

foreign import zipWithImpl :: forall a b c. (a -> b -> c) -> List a -> List b -> List c = "call 'lists':'zipwith'(fun (A, B) -> apply $0 (A, B), $1, $2)"

-- Append element to end of list (snoc)
snoc :: forall a. List a -> a -> List a
snoc xs x = snocImpl xs x

foreign import snocImpl :: forall a. List a -> a -> List a = "call 'lists':'append'($0, [$1])"
