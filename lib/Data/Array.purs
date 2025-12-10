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

-- Get element at index
index :: forall a. Array a -> Int -> Maybe a
index xs i = indexImpl xs i

foreign import indexImpl :: forall a. Array a -> Int -> Maybe a

-- Find index of element (elemIndex)
elemIndex :: forall a. a -> Array a -> Maybe Int
elemIndex x xs = elemIndexImpl x xs

foreign import elemIndexImpl :: forall a. a -> Array a -> Maybe Int

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

-- Singleton array
singleton :: forall a. a -> Array a
singleton x = singletonImpl x

foreign import singletonImpl :: forall a. a -> Array a

-- Update element at index
updateAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
updateAt i x xs = updateAtImpl i x xs

foreign import updateAtImpl :: forall a. Int -> a -> Array a -> Maybe (Array a)

-- Delete element at index
deleteAt :: forall a. Int -> Array a -> Maybe (Array a)
deleteAt i xs = deleteAtImpl i xs

foreign import deleteAtImpl :: forall a. Int -> Array a -> Maybe (Array a)

-- Insert element at index
insertAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
insertAt i x xs = insertAtImpl i x xs

foreign import insertAtImpl :: forall a. Int -> a -> Array a -> Maybe (Array a)

-- Slice array from start to end indices
slice :: forall a. Int -> Int -> Array a -> Array a
slice start end xs = sliceImpl start end xs

foreign import sliceImpl :: forall a. Int -> Int -> Array a -> Array a

-- Remove duplicates
nub :: forall a. Array a -> Array a
nub xs = nubImpl xs

foreign import nubImpl :: forall a. Array a -> Array a

-- Remove duplicates with custom equality
nubBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a
nubBy f xs = nubByImpl f xs

foreign import nubByImpl :: forall a. (a -> a -> Boolean) -> Array a -> Array a

-- Group consecutive equal elements
group :: forall a. Array a -> Array (Array a)
group xs = groupImpl xs

foreign import groupImpl :: forall a. Array a -> Array (Array a)

-- Group by custom equality
groupBy :: forall a. (a -> a -> Boolean) -> Array a -> Array (Array a)
groupBy f xs = groupByImpl f xs

foreign import groupByImpl :: forall a. (a -> a -> Boolean) -> Array a -> Array (Array a)

-- Take while predicate holds
takeWhile :: forall a. (a -> Boolean) -> Array a -> Array a
takeWhile f xs = takeWhileImpl f xs

foreign import takeWhileImpl :: forall a. (a -> Boolean) -> Array a -> Array a

-- Find index of first element matching predicate
findIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex f xs = findIndexImpl f xs

foreign import findIndexImpl :: forall a. (a -> Boolean) -> Array a -> Maybe Int

-- Sort array (uses natural ordering)
sort :: forall a. Array a -> Array a
sort xs = sortImpl xs

foreign import sortImpl :: forall a. Array a -> Array a

-- Intercalate (join arrays with separator array)
intercalate :: forall a. Array a -> Array (Array a) -> Array a
intercalate sep xs = intercalateImpl sep xs

foreign import intercalateImpl :: forall a. Array a -> Array (Array a) -> Array a

-- Split at index
splitAt :: forall a. Int -> Array a -> { before :: Array a, after :: Array a }
splitAt i xs = splitAtImpl i xs

foreign import splitAtImpl :: forall a. Int -> Array a -> { before :: Array a, after :: Array a }

-- Map with Maybe (filter and map combined)
mapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Array b
mapMaybe f xs = mapMaybeImpl f xs

foreign import mapMaybeImpl :: forall a b. (a -> Maybe b) -> Array a -> Array b

-- catMaybes - filter out Nothing values
catMaybes :: forall a. Array (Maybe a) -> Array a
catMaybes xs = mapMaybe (\x -> x) xs

-- nubByEq - remove duplicates using custom equality function
nubByEq :: forall a. (a -> a -> Boolean) -> Array a -> Array a
nubByEq f xs = nubByEqImpl f xs

foreign import nubByEqImpl :: forall a. (a -> a -> Boolean) -> Array a -> Array a

-- Convert from a List to an Array
fromFoldable :: forall a. List a -> Array a
fromFoldable xs = fromFoldableImpl xs

foreign import fromFoldableImpl :: forall a. List a -> Array a

-- Convert from Array to List
toUnfoldable :: forall a. Array a -> List a
toUnfoldable xs = toUnfoldableImpl xs

foreign import toUnfoldableImpl :: forall a. Array a -> List a
