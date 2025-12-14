module Data.Map where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- Map type (k -> v mapping)
-- Foreign type backed by Erlang maps

-- Empty map
empty :: forall k v. Map k v
empty = emptyImpl

foreign import emptyImpl :: forall k v. Map k v = "call 'maps':'new'()"

-- Singleton map
singleton :: forall k v. k -> v -> Map k v
singleton k v = singletonImpl k v

foreign import singletonImpl :: forall k v. k -> v -> Map k v = "call 'maps':'from_list'([{$0, $1}])"

-- Lookup a key
lookup :: forall k v. k -> Map k v -> Maybe v
lookup k m = lookupImpl k m

foreign import lookupImpl :: forall k v. k -> Map k v -> Maybe v = "case call 'maps':'find'($0, $1) of\n        <{'ok', V}> when 'true' -> {'Just', V}\n        <'error'> when 'true' -> 'Nothing'\n      end"

-- Check if key exists
member :: forall k v. k -> Map k v -> Boolean
member k m = memberImpl k m

foreign import memberImpl :: forall k v. k -> Map k v -> Boolean = "call 'maps':'is_key'($0, $1)"

-- Insert key-value pair
insert :: forall k v. k -> v -> Map k v -> Map k v
insert k v m = insertImpl k v m

foreign import insertImpl :: forall k v. k -> v -> Map k v -> Map k v = "call 'maps':'put'($0, $1, $2)"

-- Delete a key
delete :: forall k v. k -> Map k v -> Map k v
delete k m = deleteImpl k m

foreign import deleteImpl :: forall k v. k -> Map k v -> Map k v = "call 'maps':'remove'($0, $1)"

-- Update a key with a function
update :: forall k v. (v -> Maybe v) -> k -> Map k v -> Map k v
update f k m = updateImpl f k m

foreign import updateImpl :: forall k v. (v -> Maybe v) -> k -> Map k v -> Map k v = "case call 'maps':'find'($1, $2) of\n        <'error'> when 'true' -> $2\n        <{'ok', V}> when 'true' -> case apply $0 (V) of\n          <'Nothing'> when 'true' -> call 'maps':'remove'($1, $2)\n          <{'Just', NewV}> when 'true' -> call 'maps':'put'($1, NewV, $2)\n        end\n      end"

-- Alter (insert, update, or delete)
alter :: forall k v. (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k m = alterImpl f k m

foreign import alterImpl :: forall k v. (Maybe v -> Maybe v) -> k -> Map k v -> Map k v = "let <MaybeV> = case call 'maps':'find'($1, $2) of\n        <'error'> when 'true' -> 'Nothing'\n        <{'ok', V}> when 'true' -> {'Just', V}\n      end in case apply $0 (MaybeV) of\n        <'Nothing'> when 'true' -> call 'maps':'remove'($1, $2)\n        <{'Just', NewV}> when 'true' -> call 'maps':'put'($1, NewV, $2)\n      end"

-- Get all keys
keys :: forall k v. Map k v -> Array k
keys m = keysImpl m

foreign import keysImpl :: forall k v. Map k v -> Array k = "call 'maps':'keys'($0)"

-- Get all values
values :: forall k v. Map k v -> Array v
values m = valuesImpl m

foreign import valuesImpl :: forall k v. Map k v -> Array v = "call 'maps':'values'($0)"

-- Get size
size :: forall k v. Map k v -> Int
size m = sizeImpl m

foreign import sizeImpl :: forall k v. Map k v -> Int = "call 'maps':'size'($0)"

-- Check if empty
isEmpty :: forall k v. Map k v -> Boolean
isEmpty m = isEmptyImpl m

foreign import isEmptyImpl :: forall k v. Map k v -> Boolean = "call 'erlang':'=='(call 'maps':'size'($0), 0)"

-- Convert from array of tuples
fromFoldable :: forall k v. Array (Tuple k v) -> Map k v
fromFoldable xs = fromFoldableImpl xs

foreign import fromFoldableImpl :: forall k v. Array (Tuple k v) -> Map k v = "call 'maps':'from_list'(call 'lists':'map'(fun (_T) -> case _T of <{'Tuple', _K, _V}> when 'true' -> {_K, _V} end, $0))"

-- Convert to array of tuples
toUnfoldable :: forall k v. Map k v -> Array (Tuple k v)
toUnfoldable m = toUnfoldableImpl m

foreign import toUnfoldableImpl :: forall k v. Map k v -> Array (Tuple k v) = "call 'lists':'map'(fun (_T) -> case _T of <{_K, _V}> when 'true' -> {'Tuple', _K, _V} end, call 'maps':'to_list'($0))"

-- Union of two maps (left-biased, like PureScript)
-- Values from the first map take precedence on key conflicts
union :: forall k v. Map k v -> Map k v -> Map k v
union m1 m2 = unionImpl m1 m2

-- Note: Erlang maps:merge is right-biased, so we swap args to get left-biased behavior
foreign import unionImpl :: forall k v. Map k v -> Map k v -> Map k v = "call 'maps':'merge'($1, $0)"

-- Union with custom combining function
-- f receives (value-from-m1, value-from-m2) for conflicting keys
unionWith :: forall k v. (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f m1 m2 = unionWithImpl f m1 m2

-- Note: With swapped args, V1 is from m2 and V2 is from m1, so we apply f(V2, V1) to get f(m1-val, m2-val)
foreign import unionWithImpl :: forall k v. (v -> v -> v) -> Map k v -> Map k v -> Map k v = "call 'maps':'merge'(fun (_K, V1, V2) -> let <F1> = apply $0 (V2) in apply F1 (V1), $1, $0)"

-- Intersection of two maps
intersection :: forall k v. Map k v -> Map k v -> Map k v
intersection m1 m2 = intersectionImpl m1 m2

foreign import intersectionImpl :: forall k v. Map k v -> Map k v -> Map k v = "call 'maps':'with'(call 'maps':'keys'($1), $0)"

-- Difference of two maps
difference :: forall k v. Map k v -> Map k v -> Map k v
difference m1 m2 = differenceImpl m1 m2

foreign import differenceImpl :: forall k v. Map k v -> Map k v -> Map k v = "call 'maps':'without'(call 'maps':'keys'($1), $0)"

-- Map over values
map :: forall k a b. (a -> b) -> Map k a -> Map k b
map f m = mapImpl f m

foreign import mapImpl :: forall k a b. (a -> b) -> Map k a -> Map k b = "call 'maps':'map'(fun (_K, V) -> apply $0 (V), $1)"

-- Map over key-value pairs
mapWithKey :: forall k a b. (k -> a -> b) -> Map k a -> Map k b
mapWithKey f m = mapWithKeyImpl f m

foreign import mapWithKeyImpl :: forall k a b. (k -> a -> b) -> Map k a -> Map k b = "call 'maps':'map'(fun (K, V) -> let <F1> = apply $0 (K) in apply F1 (V), $1)"

-- Filter by value
filter :: forall k v. (v -> Boolean) -> Map k v -> Map k v
filter f m = filterImpl f m

foreign import filterImpl :: forall k v. (v -> Boolean) -> Map k v -> Map k v = "call 'maps':'filter'(fun (_K, V) -> apply $0 (V), $1)"

-- Filter by key and value
filterWithKey :: forall k v. (k -> v -> Boolean) -> Map k v -> Map k v
filterWithKey f m = filterWithKeyImpl f m

foreign import filterWithKeyImpl :: forall k v. (k -> v -> Boolean) -> Map k v -> Map k v = "call 'maps':'filter'(fun (K, V) -> let <F1> = apply $0 (K) in apply F1 (V), $1)"

-- Fold left over values
foldl :: forall k v a. (a -> v -> a) -> a -> Map k v -> a
foldl f acc m = foldlImpl f acc m

foreign import foldlImpl :: forall k v a. (a -> v -> a) -> a -> Map k v -> a = "call 'maps':'fold'(fun (_K, V, A) -> let <F1> = apply $0 (A) in apply F1 (V), $1, $2)"

-- Fold left with key
foldlWithKey :: forall k v a. (a -> k -> v -> a) -> a -> Map k v -> a
foldlWithKey f acc m = foldlWithKeyImpl f acc m

foreign import foldlWithKeyImpl :: forall k v a. (a -> k -> v -> a) -> a -> Map k v -> a = "call 'maps':'fold'(fun (K, V, A) -> let <F1> = apply $0 (A) in let <F2> = apply F1 (K) in apply F2 (V), $1, $2)"

-- Fold right over values
foldr :: forall k v a. (v -> a -> a) -> a -> Map k v -> a
foldr f acc m = foldrImpl f acc m

foreign import foldrImpl :: forall k v a. (v -> a -> a) -> a -> Map k v -> a = "call 'lists':'foldr'(fun (KV, A) -> case KV of <{_K, V}> when 'true' -> let <F1> = apply $0 (V) in apply F1 (A) end, $1, call 'maps':'to_list'($2))"

-- Find minimum key
findMin :: forall k v. Map k v -> Maybe (Tuple k v)
findMin m = findMinImpl m

foreign import findMinImpl :: forall k v. Map k v -> Maybe (Tuple k v) = "case call 'maps':'size'($0) of\n        <0> when 'true' -> 'Nothing'\n        <_> when 'true' -> let <Keys> = call 'lists':'sort'(call 'maps':'keys'($0)) in\n          let <K> = call 'erlang':'hd'(Keys) in\n            {'Just', {'Tuple', K, call 'maps':'get'(K, $0)}}\n      end"

-- Find maximum key
findMax :: forall k v. Map k v -> Maybe (Tuple k v)
findMax m = findMaxImpl m

foreign import findMaxImpl :: forall k v. Map k v -> Maybe (Tuple k v) = "case call 'maps':'size'($0) of\n        <0> when 'true' -> 'Nothing'\n        <_> when 'true' -> let <Keys> = call 'lists':'sort'(call 'maps':'keys'($0)) in\n          let <K> = call 'lists':'last'(Keys) in\n            {'Just', {'Tuple', K, call 'maps':'get'(K, $0)}}\n      end"

-- Lookup with default
lookupDefault :: forall k v. v -> k -> Map k v -> v
lookupDefault def k m = lookupDefaultImpl def k m

foreign import lookupDefaultImpl :: forall k v. v -> k -> Map k v -> v = "case call 'maps':'find'($1, $2) of\n        <{'ok', V}> when 'true' -> V\n        <'error'> when 'true' -> $0\n      end"

-- Map over values with Maybe, filtering out Nothing results
mapMaybe :: forall k a b. (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f m = mapMaybeImpl f m

foreign import mapMaybeImpl :: forall k a b. (a -> Maybe b) -> Map k a -> Map k b = "call 'maps':'fold'(fun (K, V, Acc) -> case apply $0 (V) of\n        <'Nothing'> when 'true' -> Acc\n        <{'Just', NewV}> when 'true' -> call 'maps':'put'(K, NewV, Acc)\n      end, call 'maps':'new'(), $1)"
