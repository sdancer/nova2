module Data.Array where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- Array operations
-- Most of these are foreign imports that map to runtime

-- Get head (first element)
head :: forall a. Array a -> Maybe a
head xs = headImpl xs

foreign import headImpl :: forall a. Array a -> Maybe a

-- Get last element
last :: forall a. Array a -> Maybe a
last xs = lastImpl xs

foreign import lastImpl :: forall a. Array a -> Maybe a

-- Get tail (all but first)
tail :: forall a. Array a -> Maybe (Array a)
tail xs = tailImpl xs

foreign import tailImpl :: forall a. Array a -> Maybe (Array a)

-- Get init (all but last)
init :: forall a. Array a -> Maybe (Array a)
init xs = initImpl xs

foreign import initImpl :: forall a. Array a -> Maybe (Array a)

-- Uncons: split into head and tail
uncons :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
uncons xs = unconsImpl xs

foreign import unconsImpl :: forall a. Array a -> Maybe { head :: a, tail :: Array a }

-- Length
length :: forall a. Array a -> Int
length xs = lengthImpl xs

foreign import lengthImpl :: forall a. Array a -> Int

-- Check if empty
null :: forall a. Array a -> Boolean
null xs = nullImpl xs

foreign import nullImpl :: forall a. Array a -> Boolean

-- Check membership
elem :: forall a. a -> Array a -> Boolean
elem x xs = elemImpl x xs

foreign import elemImpl :: forall a. a -> Array a -> Boolean

-- Prepend element
cons :: forall a. a -> Array a -> Array a
cons x xs = consImpl x xs

foreign import consImpl :: forall a. a -> Array a -> Array a

-- Append element
snoc :: forall a. Array a -> a -> Array a
snoc xs x = snocImpl xs x

foreign import snocImpl :: forall a. Array a -> a -> Array a

-- Take first n elements
take :: forall a. Int -> Array a -> Array a
take n xs = takeImpl n xs

foreign import takeImpl :: forall a. Int -> Array a -> Array a

-- Drop first n elements
drop :: forall a. Int -> Array a -> Array a
drop n xs = dropImpl n xs

foreign import dropImpl :: forall a. Int -> Array a -> Array a

-- Reverse
reverse :: forall a. Array a -> Array a
reverse xs = reverseImpl xs

foreign import reverseImpl :: forall a. Array a -> Array a

-- Filter
filter :: forall a. (a -> Boolean) -> Array a -> Array a
filter f xs = filterImpl f xs

foreign import filterImpl :: forall a. (a -> Boolean) -> Array a -> Array a

-- Find first matching
find :: forall a. (a -> Boolean) -> Array a -> Maybe a
find f xs = findImpl f xs

foreign import findImpl :: forall a. (a -> Boolean) -> Array a -> Maybe a

-- Find index
index :: forall a. a -> Array a -> Maybe Int
index x xs = indexImpl x xs

foreign import indexImpl :: forall a. a -> Array a -> Maybe Int

-- Map
map :: forall a b. (a -> b) -> Array a -> Array b
map f xs = mapImpl f xs

foreign import mapImpl :: forall a b. (a -> b) -> Array a -> Array b

-- Map with index
mapWithIndex :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapWithIndex f xs = mapWithIndexImpl f xs

foreign import mapWithIndexImpl :: forall a b. (Int -> a -> b) -> Array a -> Array b

-- Fold left
foldl :: forall a b. (b -> a -> b) -> b -> Array a -> b
foldl f acc xs = foldlImpl f acc xs

foreign import foldlImpl :: forall a b. (b -> a -> b) -> b -> Array a -> b

-- Fold right
foldr :: forall a b. (a -> b -> b) -> b -> Array a -> b
foldr f acc xs = foldrImpl f acc xs

foreign import foldrImpl :: forall a b. (a -> b -> b) -> b -> Array a -> b

-- Zip two arrays
zip :: forall a b. Array a -> Array b -> Array (Tuple a b)
zip xs ys = zipImpl xs ys

foreign import zipImpl :: forall a b. Array a -> Array b -> Array (Tuple a b)

-- Zip with function
zipWith :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
zipWith f xs ys = zipWithImpl f xs ys

foreign import zipWithImpl :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c

-- Concatenate arrays
concat :: forall a. Array (Array a) -> Array a
concat xs = concatImpl xs

foreign import concatImpl :: forall a. Array (Array a) -> Array a

-- ConcatMap (flatMap)
concatMap :: forall a b. (a -> Array b) -> Array a -> Array b
concatMap f xs = concatMapImpl f xs

foreign import concatMapImpl :: forall a b. (a -> Array b) -> Array a -> Array b

-- Range
range :: Int -> Int -> Array Int
range a b = rangeImpl a b

foreign import rangeImpl :: Int -> Int -> Array Int

-- Replicate
replicate :: forall a. Int -> a -> Array a
replicate n x = replicateImpl n x

foreign import replicateImpl :: forall a. Int -> a -> Array a

-- Sort by key
sortBy :: forall a b. (a -> b) -> Array a -> Array a
sortBy f xs = sortByImpl f xs

foreign import sortByImpl :: forall a b. (a -> b) -> Array a -> Array a

-- All elements satisfy predicate
all :: forall a. (a -> Boolean) -> Array a -> Boolean
all f xs = allImpl f xs

foreign import allImpl :: forall a. (a -> Boolean) -> Array a -> Boolean

-- Any element satisfies predicate
any :: forall a. (a -> Boolean) -> Array a -> Boolean
any f xs = anyImpl f xs

foreign import anyImpl :: forall a. (a -> Boolean) -> Array a -> Boolean

-- Drop while predicate holds
dropWhile :: forall a. (a -> Boolean) -> Array a -> Array a
dropWhile f xs = dropWhileImpl f xs

foreign import dropWhileImpl :: forall a. (a -> Boolean) -> Array a -> Array a

-- Span: split at first element not satisfying predicate
span :: forall a. (a -> Boolean) -> Array a -> { init :: Array a, rest :: Array a }
span f xs = spanImpl f xs

foreign import spanImpl :: forall a. (a -> Boolean) -> Array a -> { init :: Array a, rest :: Array a }

-- Partition by predicate
partition :: forall a. (a -> Boolean) -> Array a -> { yes :: Array a, no :: Array a }
partition f xs = partitionImpl f xs

foreign import partitionImpl :: forall a. (a -> Boolean) -> Array a -> { yes :: Array a, no :: Array a }
