module Data.Set where

import Prelude
import Data.Maybe (Maybe(..))

-- Set type (collection of unique values)
-- Foreign type backed by Erlang sets module

-- Empty set
empty :: forall a. Set a
empty = emptyImpl

foreign import emptyImpl :: forall a. Set a = "call 'sets':'new'()"

-- Singleton set
singleton :: forall a. a -> Set a
singleton x = singletonImpl x

foreign import singletonImpl :: forall a. a -> Set a = "call 'sets':'from_list'([$0])"

-- Check membership
member :: forall a. a -> Set a -> Boolean
member x s = memberImpl x s

foreign import memberImpl :: forall a. a -> Set a -> Boolean = "call 'sets':'is_element'($0, $1)"

-- Insert element
insert :: forall a. a -> Set a -> Set a
insert x s = insertImpl x s

foreign import insertImpl :: forall a. a -> Set a -> Set a = "call 'sets':'add_element'($0, $1)"

-- Delete element
delete :: forall a. a -> Set a -> Set a
delete x s = deleteImpl x s

foreign import deleteImpl :: forall a. a -> Set a -> Set a = "call 'sets':'del_element'($0, $1)"

-- Get size
size :: forall a. Set a -> Int
size s = sizeImpl s

foreign import sizeImpl :: forall a. Set a -> Int = "call 'sets':'size'($0)"

-- Check if empty
isEmpty :: forall a. Set a -> Boolean
isEmpty s = isEmptyImpl s

foreign import isEmptyImpl :: forall a. Set a -> Boolean = "call 'erlang':'=='(call 'sets':'size'($0), 0)"

-- Convert from array
fromFoldable :: forall a. Array a -> Set a
fromFoldable xs = fromFoldableImpl xs

foreign import fromFoldableImpl :: forall a. Array a -> Set a = "call 'sets':'from_list'($0)"

-- Convert to array
toUnfoldable :: forall a. Set a -> Array a
toUnfoldable s = toUnfoldableImpl s

foreign import toUnfoldableImpl :: forall a. Set a -> Array a = "call 'sets':'to_list'($0)"

-- Union of two sets
union :: forall a. Set a -> Set a -> Set a
union s1 s2 = unionImpl s1 s2

foreign import unionImpl :: forall a. Set a -> Set a -> Set a = "call 'sets':'union'($0, $1)"

-- Intersection of two sets
intersection :: forall a. Set a -> Set a -> Set a
intersection s1 s2 = intersectionImpl s1 s2

foreign import intersectionImpl :: forall a. Set a -> Set a -> Set a = "call 'sets':'intersection'($0, $1)"

-- Difference of two sets
difference :: forall a. Set a -> Set a -> Set a
difference s1 s2 = differenceImpl s1 s2

foreign import differenceImpl :: forall a. Set a -> Set a -> Set a = "call 'sets':'subtract'($0, $1)"

-- Subset check
subset :: forall a. Set a -> Set a -> Boolean
subset s1 s2 = subsetImpl s1 s2

foreign import subsetImpl :: forall a. Set a -> Set a -> Boolean = "call 'sets':'is_subset'($0, $1)"

-- Proper subset check
properSubset :: forall a. Set a -> Set a -> Boolean
properSubset s1 s2 = properSubsetImpl s1 s2

foreign import properSubsetImpl :: forall a. Set a -> Set a -> Boolean = "call 'erlang':'and'(call 'sets':'is_subset'($0, $1), call 'erlang':'<'(call 'sets':'size'($0), call 'sets':'size'($1)))"

-- Map over set
map :: forall a b. (a -> b) -> Set a -> Set b
map f s = mapImpl f s

foreign import mapImpl :: forall a b. (a -> b) -> Set a -> Set b = "call 'sets':'from_list'(call 'lists':'map'($0, call 'sets':'to_list'($1)))"

-- Filter set
filter :: forall a. (a -> Boolean) -> Set a -> Set a
filter f s = filterImpl f s

foreign import filterImpl :: forall a. (a -> Boolean) -> Set a -> Set a = "call 'sets':'filter'($0, $1)"

-- Fold left
foldl :: forall a b. (b -> a -> b) -> b -> Set a -> b
foldl f acc s = foldlImpl f acc s

foreign import foldlImpl :: forall a b. (b -> a -> b) -> b -> Set a -> b = "call 'sets':'fold'(fun (E, A) -> let <F1> = apply $0 (A) in apply F1 (E), $1, $2)"

-- Fold right
foldr :: forall a b. (a -> b -> b) -> b -> Set a -> b
foldr f acc s = foldrImpl f acc s

foreign import foldrImpl :: forall a b. (a -> b -> b) -> b -> Set a -> b = "call 'lists':'foldr'(fun (E, A) -> let <F1> = apply $0 (E) in apply F1 (A), $1, call 'sets':'to_list'($2))"

-- Find minimum
findMin :: forall a. Set a -> Maybe a
findMin s = findMinImpl s

foreign import findMinImpl :: forall a. Set a -> Maybe a = "case call 'sets':'size'($0) of\n        <0> when 'true' -> 'Nothing'\n        <_> when 'true' -> {'Just', call 'lists':'min'(call 'sets':'to_list'($0))}\n      end"

-- Find maximum
findMax :: forall a. Set a -> Maybe a
findMax s = findMaxImpl s

foreign import findMaxImpl :: forall a. Set a -> Maybe a = "case call 'sets':'size'($0) of\n        <0> when 'true' -> 'Nothing'\n        <_> when 'true' -> {'Just', call 'lists':'max'(call 'sets':'to_list'($0))}\n      end"

-- Toggle membership (insert if absent, delete if present)
toggle :: forall a. a -> Set a -> Set a
toggle x s = toggleImpl x s

foreign import toggleImpl :: forall a. a -> Set a -> Set a = "case call 'sets':'is_element'($0, $1) of\n        <'true'> when 'true' -> call 'sets':'del_element'($0, $1)\n        <'false'> when 'true' -> call 'sets':'add_element'($0, $1)\n      end"

-- Union of multiple sets
unions :: forall a. Array (Set a) -> Set a
unions sets = unionsImpl sets

foreign import unionsImpl :: forall a. Array (Set a) -> Set a = "call 'lists':'foldl'(fun (S, Acc) -> call 'sets':'union'(S, Acc), call 'sets':'new'(), $0)"

-- Map and filter in one pass (like catMaybes . map)
mapMaybe :: forall a b. (a -> Maybe b) -> Set a -> Set b
mapMaybe f s = foldl (\acc x -> case f x of
  Just y -> insert y acc
  Nothing -> acc) empty s

-- Eq instance for Set - compare two sets for equality
-- Two sets are equal if they contain the same elements
instance eqSet :: Eq a => Eq (Set a) where
  eq s1 s2 = eqSetImpl s1 s2

foreign import eqSetImpl :: forall a. Set a -> Set a -> Boolean = "call 'erlang':'=='($0, $1)"
