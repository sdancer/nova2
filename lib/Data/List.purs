module Data.List where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- List operations
-- These are foreign imports that map to Nova.List runtime

-- Convert from foldable (Array) to List
fromFoldable :: forall a. Array a -> List a
fromFoldable xs = fromFoldableImpl xs

foreign import fromFoldableImpl :: forall a. Array a -> List a

-- Convert to unfoldable (Array)
toUnfoldable :: forall a. List a -> Array a
toUnfoldable xs = toUnfoldableImpl xs

foreign import toUnfoldableImpl :: forall a. List a -> Array a

-- Check if list is null/empty
null :: forall a. List a -> Boolean
null xs = nullImpl xs

foreign import nullImpl :: forall a. List a -> Boolean

-- Get head and tail as record
uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons xs = unconsImpl xs

foreign import unconsImpl :: forall a. List a -> Maybe { head :: a, tail :: List a }

-- Get head (first element)
head :: forall a. List a -> Maybe a
head xs = headImpl xs

foreign import headImpl :: forall a. List a -> Maybe a

-- Get tail (all but first)
tail :: forall a. List a -> Maybe (List a)
tail xs = tailImpl xs

foreign import tailImpl :: forall a. List a -> Maybe (List a)

-- Reverse a list
reverse :: forall a. List a -> List a
reverse xs = reverseImpl xs

foreign import reverseImpl :: forall a. List a -> List a

-- Length of a list
length :: forall a. List a -> Int
length xs = lengthImpl xs

foreign import lengthImpl :: forall a. List a -> Int

-- Prepend element (cons)
cons :: forall a. a -> List a -> List a
cons x xs = consImpl x xs

foreign import consImpl :: forall a. a -> List a -> List a

-- Create singleton list
singleton :: forall a. a -> List a
singleton x = singletonImpl x

foreign import singletonImpl :: forall a. a -> List a

-- Append two lists
append :: forall a. List a -> List a -> List a
append xs ys = appendImpl xs ys

foreign import appendImpl :: forall a. List a -> List a -> List a

-- Map over a list
map :: forall a b. (a -> b) -> List a -> List b
map f xs = mapImpl f xs

foreign import mapImpl :: forall a b. (a -> b) -> List a -> List b

-- Filter a list
filter :: forall a. (a -> Boolean) -> List a -> List a
filter f xs = filterImpl f xs

foreign import filterImpl :: forall a. (a -> Boolean) -> List a -> List a

-- Fold right
foldr :: forall a b. (a -> b -> b) -> b -> List a -> b
foldr f acc xs = foldrImpl f acc xs

foreign import foldrImpl :: forall a b. (a -> b -> b) -> b -> List a -> b

-- Fold left
foldl :: forall a b. (b -> a -> b) -> b -> List a -> b
foldl f acc xs = foldlImpl f acc xs

foreign import foldlImpl :: forall a b. (b -> a -> b) -> b -> List a -> b

-- Take first n elements
take :: forall a. Int -> List a -> List a
take n xs = takeImpl n xs

foreign import takeImpl :: forall a. Int -> List a -> List a

-- Drop first n elements
drop :: forall a. Int -> List a -> List a
drop n xs = dropImpl n xs

foreign import dropImpl :: forall a. Int -> List a -> List a

-- Any element satisfies predicate
any :: forall a. (a -> Boolean) -> List a -> Boolean
any f xs = anyImpl f xs

foreign import anyImpl :: forall a. (a -> Boolean) -> List a -> Boolean

-- Map with Maybe filter (keep only Just values)
mapMaybe :: forall a b. (a -> Maybe b) -> List a -> List b
mapMaybe f xs = mapMaybeImpl f xs

foreign import mapMaybeImpl :: forall a b. (a -> Maybe b) -> List a -> List b

-- Take while predicate holds
takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile f xs = takeWhileImpl f xs

foreign import takeWhileImpl :: forall a. (a -> Boolean) -> List a -> List a

-- Drop while predicate holds
dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile f xs = dropWhileImpl f xs

foreign import dropWhileImpl :: forall a. (a -> Boolean) -> List a -> List a

-- Create a range from start to end (inclusive)
range :: Int -> Int -> List Int
range start end = rangeImpl start end

foreign import rangeImpl :: Int -> Int -> List Int

-- Check if element is in list
elem :: forall a. Eq a => a -> List a -> Boolean
elem x xs = elemImpl x xs

foreign import elemImpl :: forall a. a -> List a -> Boolean
