module Data.Set where

import Data.Maybe (Maybe(..))

-- Set type (collection of unique values)
-- Foreign type backed by Elixir MapSet

-- Empty set
empty :: forall a. Set a
empty = emptyImpl

foreign import emptyImpl :: forall a. Set a

-- Singleton set
singleton :: forall a. a -> Set a
singleton x = singletonImpl x

foreign import singletonImpl :: forall a. a -> Set a

-- Check membership
member :: forall a. a -> Set a -> Boolean
member x s = memberImpl x s

foreign import memberImpl :: forall a. a -> Set a -> Boolean

-- Insert element
insert :: forall a. a -> Set a -> Set a
insert x s = insertImpl x s

foreign import insertImpl :: forall a. a -> Set a -> Set a

-- Delete element
delete :: forall a. a -> Set a -> Set a
delete x s = deleteImpl x s

foreign import deleteImpl :: forall a. a -> Set a -> Set a

-- Get size
size :: forall a. Set a -> Int
size s = sizeImpl s

foreign import sizeImpl :: forall a. Set a -> Int

-- Check if empty
isEmpty :: forall a. Set a -> Boolean
isEmpty s = isEmptyImpl s

foreign import isEmptyImpl :: forall a. Set a -> Boolean

-- Convert from array
fromFoldable :: forall a. Array a -> Set a
fromFoldable xs = fromFoldableImpl xs

foreign import fromFoldableImpl :: forall a. Array a -> Set a

-- Convert to array
toUnfoldable :: forall a. Set a -> Array a
toUnfoldable s = toUnfoldableImpl s

foreign import toUnfoldableImpl :: forall a. Set a -> Array a

-- Union of two sets
union :: forall a. Set a -> Set a -> Set a
union s1 s2 = unionImpl s1 s2

foreign import unionImpl :: forall a. Set a -> Set a -> Set a

-- Intersection of two sets
intersection :: forall a. Set a -> Set a -> Set a
intersection s1 s2 = intersectionImpl s1 s2

foreign import intersectionImpl :: forall a. Set a -> Set a -> Set a

-- Difference of two sets
difference :: forall a. Set a -> Set a -> Set a
difference s1 s2 = differenceImpl s1 s2

foreign import differenceImpl :: forall a. Set a -> Set a -> Set a

-- Subset check
subset :: forall a. Set a -> Set a -> Boolean
subset s1 s2 = subsetImpl s1 s2

foreign import subsetImpl :: forall a. Set a -> Set a -> Boolean

-- Proper subset check
properSubset :: forall a. Set a -> Set a -> Boolean
properSubset s1 s2 = properSubsetImpl s1 s2

foreign import properSubsetImpl :: forall a. Set a -> Set a -> Boolean

-- Map over set
map :: forall a b. (a -> b) -> Set a -> Set b
map f s = mapImpl f s

foreign import mapImpl :: forall a b. (a -> b) -> Set a -> Set b

-- Filter set
filter :: forall a. (a -> Boolean) -> Set a -> Set a
filter f s = filterImpl f s

foreign import filterImpl :: forall a. (a -> Boolean) -> Set a -> Set a

-- Fold left
foldl :: forall a b. (b -> a -> b) -> b -> Set a -> b
foldl f acc s = foldlImpl f acc s

foreign import foldlImpl :: forall a b. (b -> a -> b) -> b -> Set a -> b

-- Fold right
foldr :: forall a b. (a -> b -> b) -> b -> Set a -> b
foldr f acc s = foldrImpl f acc s

foreign import foldrImpl :: forall a b. (a -> b -> b) -> b -> Set a -> b

-- Find minimum
findMin :: forall a. Set a -> Maybe a
findMin s = findMinImpl s

foreign import findMinImpl :: forall a. Set a -> Maybe a

-- Find maximum
findMax :: forall a. Set a -> Maybe a
findMax s = findMaxImpl s

foreign import findMaxImpl :: forall a. Set a -> Maybe a

-- Toggle membership (insert if absent, delete if present)
toggle :: forall a. a -> Set a -> Set a
toggle x s = toggleImpl x s

foreign import toggleImpl :: forall a. a -> Set a -> Set a

-- Union of multiple sets
unions :: forall a. Array (Set a) -> Set a
unions sets = unionsImpl sets

foreign import unionsImpl :: forall a. Array (Set a) -> Set a

-- Map and filter in one pass (like catMaybes . map)
mapMaybe :: forall a b. (a -> Maybe b) -> Set a -> Set b
mapMaybe f s = foldl (\acc x -> case f x of
  Just y -> insert y acc
  Nothing -> acc) empty s
