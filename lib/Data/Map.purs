module Data.Map where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- Map type (k -> v mapping)
-- Foreign type backed by Elixir Map

-- Empty map
empty :: forall k v. Map k v
empty = emptyImpl

foreign import emptyImpl :: forall k v. Map k v

-- Singleton map
singleton :: forall k v. k -> v -> Map k v
singleton k v = singletonImpl k v

foreign import singletonImpl :: forall k v. k -> v -> Map k v

-- Lookup a key
lookup :: forall k v. k -> Map k v -> Maybe v
lookup k m = lookupImpl k m

foreign import lookupImpl :: forall k v. k -> Map k v -> Maybe v

-- Check if key exists
member :: forall k v. k -> Map k v -> Boolean
member k m = memberImpl k m

foreign import memberImpl :: forall k v. k -> Map k v -> Boolean

-- Insert key-value pair
insert :: forall k v. k -> v -> Map k v -> Map k v
insert k v m = insertImpl k v m

foreign import insertImpl :: forall k v. k -> v -> Map k v -> Map k v

-- Delete a key
delete :: forall k v. k -> Map k v -> Map k v
delete k m = deleteImpl k m

foreign import deleteImpl :: forall k v. k -> Map k v -> Map k v

-- Update a key with a function
update :: forall k v. (v -> Maybe v) -> k -> Map k v -> Map k v
update f k m = updateImpl f k m

foreign import updateImpl :: forall k v. (v -> Maybe v) -> k -> Map k v -> Map k v

-- Alter (insert, update, or delete)
alter :: forall k v. (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k m = alterImpl f k m

foreign import alterImpl :: forall k v. (Maybe v -> Maybe v) -> k -> Map k v -> Map k v

-- Get all keys
keys :: forall k v. Map k v -> Array k
keys m = keysImpl m

foreign import keysImpl :: forall k v. Map k v -> Array k

-- Get all values
values :: forall k v. Map k v -> Array v
values m = valuesImpl m

foreign import valuesImpl :: forall k v. Map k v -> Array v

-- Get size
size :: forall k v. Map k v -> Int
size m = sizeImpl m

foreign import sizeImpl :: forall k v. Map k v -> Int

-- Check if empty
isEmpty :: forall k v. Map k v -> Boolean
isEmpty m = isEmptyImpl m

foreign import isEmptyImpl :: forall k v. Map k v -> Boolean

-- Convert from array of tuples
fromFoldable :: forall k v. Array (Tuple k v) -> Map k v
fromFoldable xs = fromFoldableImpl xs

foreign import fromFoldableImpl :: forall k v. Array (Tuple k v) -> Map k v

-- Convert to array of tuples
toUnfoldable :: forall k v. Map k v -> Array (Tuple k v)
toUnfoldable m = toUnfoldableImpl m

foreign import toUnfoldableImpl :: forall k v. Map k v -> Array (Tuple k v)

-- Union of two maps (left-biased)
union :: forall k v. Map k v -> Map k v -> Map k v
union m1 m2 = unionImpl m1 m2

foreign import unionImpl :: forall k v. Map k v -> Map k v -> Map k v

-- Union with custom combining function
unionWith :: forall k v. (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f m1 m2 = unionWithImpl f m1 m2

foreign import unionWithImpl :: forall k v. (v -> v -> v) -> Map k v -> Map k v -> Map k v

-- Intersection of two maps
intersection :: forall k v. Map k v -> Map k v -> Map k v
intersection m1 m2 = intersectionImpl m1 m2

foreign import intersectionImpl :: forall k v. Map k v -> Map k v -> Map k v

-- Difference of two maps
difference :: forall k v. Map k v -> Map k v -> Map k v
difference m1 m2 = differenceImpl m1 m2

foreign import differenceImpl :: forall k v. Map k v -> Map k v -> Map k v

-- Map over values
map :: forall k a b. (a -> b) -> Map k a -> Map k b
map f m = mapImpl f m

foreign import mapImpl :: forall k a b. (a -> b) -> Map k a -> Map k b

-- Map over key-value pairs
mapWithKey :: forall k a b. (k -> a -> b) -> Map k a -> Map k b
mapWithKey f m = mapWithKeyImpl f m

foreign import mapWithKeyImpl :: forall k a b. (k -> a -> b) -> Map k a -> Map k b

-- Filter by value
filter :: forall k v. (v -> Boolean) -> Map k v -> Map k v
filter f m = filterImpl f m

foreign import filterImpl :: forall k v. (v -> Boolean) -> Map k v -> Map k v

-- Filter by key and value
filterWithKey :: forall k v. (k -> v -> Boolean) -> Map k v -> Map k v
filterWithKey f m = filterWithKeyImpl f m

foreign import filterWithKeyImpl :: forall k v. (k -> v -> Boolean) -> Map k v -> Map k v

-- Fold left over values
foldl :: forall k v a. (a -> v -> a) -> a -> Map k v -> a
foldl f acc m = foldlImpl f acc m

foreign import foldlImpl :: forall k v a. (a -> v -> a) -> a -> Map k v -> a

-- Fold left with key
foldlWithKey :: forall k v a. (a -> k -> v -> a) -> a -> Map k v -> a
foldlWithKey f acc m = foldlWithKeyImpl f acc m

foreign import foldlWithKeyImpl :: forall k v a. (a -> k -> v -> a) -> a -> Map k v -> a

-- Fold right over values
foldr :: forall k v a. (v -> a -> a) -> a -> Map k v -> a
foldr f acc m = foldrImpl f acc m

foreign import foldrImpl :: forall k v a. (v -> a -> a) -> a -> Map k v -> a

-- Find minimum key
findMin :: forall k v. Map k v -> Maybe (Tuple k v)
findMin m = findMinImpl m

foreign import findMinImpl :: forall k v. Map k v -> Maybe (Tuple k v)

-- Find maximum key
findMax :: forall k v. Map k v -> Maybe (Tuple k v)
findMax m = findMaxImpl m

foreign import findMaxImpl :: forall k v. Map k v -> Maybe (Tuple k v)

-- Lookup with default
lookupDefault :: forall k v. v -> k -> Map k v -> v
lookupDefault def k m = lookupDefaultImpl def k m

foreign import lookupDefaultImpl :: forall k v. v -> k -> Map k v -> v
