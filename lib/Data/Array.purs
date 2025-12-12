module Data.Array where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- Array operations
-- Most of these are foreign imports that map to runtime

-- Get head (first element)
head :: forall a. Array a -> Maybe a
head xs = headImpl xs

foreign import headImpl :: forall a. Array a -> Maybe a = "case $0 of\n        <[]> when 'true' -> 'Nothing'\n        <[H|_]> when 'true' -> {'Just', H}\n      end"

-- Get last element
last :: forall a. Array a -> Maybe a
last xs = lastImpl xs

foreign import lastImpl :: forall a. Array a -> Maybe a = "case $0 of\n        <[]> when 'true' -> 'Nothing'\n        <_> when 'true' -> {'Just', call 'lists':'last'($0)}\n      end"

-- Get tail (all but first)
tail :: forall a. Array a -> Maybe (Array a)
tail xs = tailImpl xs

foreign import tailImpl :: forall a. Array a -> Maybe (Array a) = "case $0 of\n        <[]> when 'true' -> 'Nothing'\n        <[_|T]> when 'true' -> {'Just', T}\n      end"

-- Get init (all but last)
init :: forall a. Array a -> Maybe (Array a)
init xs = initImpl xs

foreign import initImpl :: forall a. Array a -> Maybe (Array a) = "case $0 of\n        <[]> when 'true' -> 'Nothing'\n        <_> when 'true' -> {'Just', call 'lists':'droplast'($0)}\n      end"

-- Uncons: split into head and tail
uncons :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
uncons xs = unconsImpl xs

foreign import unconsImpl :: forall a. Array a -> Maybe { head :: a, tail :: Array a } = "case $0 of\n        <[]> when 'true' -> 'Nothing'\n        <[H|T]> when 'true' -> {'Just', ~{'head'=>H,'tail'=>T}~}\n      end"

-- Length
length :: forall a. Array a -> Int
length xs = lengthImpl xs

foreign import lengthImpl :: forall a. Array a -> Int = "call 'erlang':'length'($0)"

-- Check if empty
null :: forall a. Array a -> Boolean
null xs = nullImpl xs

foreign import nullImpl :: forall a. Array a -> Boolean = "case $0 of\n        <[]> when 'true' -> 'true'\n        <_> when 'true' -> 'false'\n      end"

-- Check membership
elem :: forall a. a -> Array a -> Boolean
elem x xs = elemImpl x xs

foreign import elemImpl :: forall a. a -> Array a -> Boolean = "call 'lists':'member'($0, $1)"

-- Prepend element
cons :: forall a. a -> Array a -> Array a
cons x xs = consImpl x xs

foreign import consImpl :: forall a. a -> Array a -> Array a = "[$0|$1]"

-- Append element
snoc :: forall a. Array a -> a -> Array a
snoc xs x = snocImpl xs x

foreign import snocImpl :: forall a. Array a -> a -> Array a = "call 'lists':'append'($0, [$1])"

-- Take first n elements
take :: forall a. Int -> Array a -> Array a
take n xs = takeImpl n xs

foreign import takeImpl :: forall a. Int -> Array a -> Array a = "call 'lists':'sublist'($1, $0)"

-- Drop first n elements
drop :: forall a. Int -> Array a -> Array a
drop n xs = dropImpl n xs

foreign import dropImpl :: forall a. Int -> Array a -> Array a = "case $0 of\n        <0> when 'true' -> $1\n        <N> when 'true' -> call 'lists':'nthtail'(N, $1)\n      end"

-- Reverse
reverse :: forall a. Array a -> Array a
reverse xs = reverseImpl xs

foreign import reverseImpl :: forall a. Array a -> Array a = "call 'lists':'reverse'($0)"

-- Filter
filter :: forall a. (a -> Boolean) -> Array a -> Array a
filter f xs = filterImpl f xs

foreign import filterImpl :: forall a. (a -> Boolean) -> Array a -> Array a = "call 'lists':'filter'($0, $1)"

-- Find first matching
find :: forall a. (a -> Boolean) -> Array a -> Maybe a
find f xs = findImpl f xs

foreign import findImpl :: forall a. (a -> Boolean) -> Array a -> Maybe a = "letrec 'doFind'/2 = fun (P, Lst) -> case Lst of\n        <[]> when 'true' -> 'Nothing'\n        <[H|T]> when 'true' -> case apply P (H) of\n          <'true'> when 'true' -> {'Just', H}\n          <'false'> when 'true' -> apply 'doFind'/2 (P, T)\n        end\n      end in apply 'doFind'/2 ($0, $1)"

-- Get element at index
index :: forall a. Array a -> Int -> Maybe a
index xs i = indexImpl xs i

foreign import indexImpl :: forall a. Array a -> Int -> Maybe a = "let <Arr> = $0 in\n        let <Idx> = $1 in\n          case call 'erlang':'or'(call 'erlang':'<'(Idx, 0), call 'erlang':'>='(Idx, call 'erlang':'length'(Arr))) of\n            <'true'> when 'true' -> 'Nothing'\n            <'false'> when 'true' -> {'Just', call 'lists':'nth'(call 'erlang':'+'(Idx, 1), Arr)}\n          end"

-- Find index of element (elemIndex)
elemIndex :: forall a. a -> Array a -> Maybe Int
elemIndex x xs = elemIndexImpl x xs

foreign import elemIndexImpl :: forall a. a -> Array a -> Maybe Int = "letrec 'findIdx'/3 = fun (Elem, N, Lst) -> case Lst of\n        <[]> when 'true' -> 'Nothing'\n        <[H|T]> when 'true' -> case call 'erlang':'=='(H, Elem) of\n          <'true'> when 'true' -> {'Just', N}\n          <'false'> when 'true' -> apply 'findIdx'/3 (Elem, call 'erlang':'+'(N, 1), T)\n        end\n      end in apply 'findIdx'/3 ($0, 0, $1)"

-- Map
map :: forall a b. (a -> b) -> Array a -> Array b
map f xs = mapImpl f xs

foreign import mapImpl :: forall a b. (a -> b) -> Array a -> Array b = "call 'lists':'map'($0, $1)"

-- Map with index
mapWithIndex :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapWithIndex f xs = mapWithIndexImpl f xs

foreign import mapWithIndexImpl :: forall a b. (Int -> a -> b) -> Array a -> Array b = "letrec 'doMapIdx'/3 = fun (F, N, Lst) -> case Lst of\n        <[]> when 'true' -> []\n        <[H|T]> when 'true' -> [apply F (N, H)|apply 'doMapIdx'/3 (F, call 'erlang':'+'(N, 1), T)]\n      end in apply 'doMapIdx'/3 ($0, 0, $1)"

-- Fold left
foldl :: forall a b. (b -> a -> b) -> b -> Array a -> b
foldl f acc xs = foldlImpl f acc xs

foreign import foldlImpl :: forall a b. (b -> a -> b) -> b -> Array a -> b = "call 'lists':'foldl'(fun (E, A) -> apply $0 (A, E), $1, $2)"

-- Fold right
foldr :: forall a b. (a -> b -> b) -> b -> Array a -> b
foldr f acc xs = foldrImpl f acc xs

foreign import foldrImpl :: forall a b. (a -> b -> b) -> b -> Array a -> b = "call 'lists':'foldr'(fun (E, A) -> apply $0 (E, A), $1, $2)"

-- Zip two arrays
zip :: forall a b. Array a -> Array b -> Array (Tuple a b)
zip xs ys = zipImpl xs ys

foreign import zipImpl :: forall a b. Array a -> Array b -> Array (Tuple a b) = "call 'lists':'zipwith'(fun (A, B) -> {'Tuple', A, B}, $0, $1)"

-- Zip with function
zipWith :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
zipWith f xs ys = zipWithImpl f xs ys

foreign import zipWithImpl :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c = "call 'lists':'zipwith'(fun (A, B) -> apply $0 (A, B), $1, $2)"

-- Concatenate arrays
concat :: forall a. Array (Array a) -> Array a
concat xs = concatImpl xs

foreign import concatImpl :: forall a. Array (Array a) -> Array a = "call 'lists':'append'($0)"

-- ConcatMap (flatMap)
concatMap :: forall a b. (a -> Array b) -> Array a -> Array b
concatMap f xs = concatMapImpl f xs

foreign import concatMapImpl :: forall a b. (a -> Array b) -> Array a -> Array b = "call 'lists':'append'(call 'lists':'map'($0, $1))"

-- Range
range :: Int -> Int -> Array Int
range a b = rangeImpl a b

foreign import rangeImpl :: Int -> Int -> Array Int = "call 'lists':'seq'($0, $1)"

-- Replicate
replicate :: forall a. Int -> a -> Array a
replicate n x = replicateImpl n x

foreign import replicateImpl :: forall a. Int -> a -> Array a = "call 'lists':'duplicate'($0, $1)"

-- Sort by key
sortBy :: forall a b. (a -> b) -> Array a -> Array a
sortBy f xs = sortByImpl f xs

foreign import sortByImpl :: forall a b. (a -> b) -> Array a -> Array a = "call 'lists':'sort'(fun (A, B) -> call 'erlang':'=<'(apply $0 (A), apply $0 (B)), $1)"

-- All elements satisfy predicate
all :: forall a. (a -> Boolean) -> Array a -> Boolean
all f xs = allImpl f xs

foreign import allImpl :: forall a. (a -> Boolean) -> Array a -> Boolean = "call 'lists':'all'($0, $1)"

-- Any element satisfies predicate
any :: forall a. (a -> Boolean) -> Array a -> Boolean
any f xs = anyImpl f xs

foreign import anyImpl :: forall a. (a -> Boolean) -> Array a -> Boolean = "call 'lists':'any'($0, $1)"

-- Drop while predicate holds
dropWhile :: forall a. (a -> Boolean) -> Array a -> Array a
dropWhile f xs = dropWhileImpl f xs

foreign import dropWhileImpl :: forall a. (a -> Boolean) -> Array a -> Array a = "call 'lists':'dropwhile'($0, $1)"

-- Span: split at first element not satisfying predicate
span :: forall a. (a -> Boolean) -> Array a -> { init :: Array a, rest :: Array a }
span f xs = spanImpl f xs

foreign import spanImpl :: forall a. (a -> Boolean) -> Array a -> { init :: Array a, rest :: Array a } = "case call 'lists':'splitwith'($0, $1) of\n        <{Taken, Rest}> when 'true' -> {'init', Taken, 'rest', Rest}\n      end"

-- Partition by predicate
partition :: forall a. (a -> Boolean) -> Array a -> { yes :: Array a, no :: Array a }
partition f xs = partitionImpl f xs

foreign import partitionImpl :: forall a. (a -> Boolean) -> Array a -> { yes :: Array a, no :: Array a } = "case call 'lists':'partition'($0, $1) of\n        <{Trues, Falses}> when 'true' -> {'yes', Trues, 'no', Falses}\n      end"

-- Singleton array
singleton :: forall a. a -> Array a
singleton x = singletonImpl x

foreign import singletonImpl :: forall a. a -> Array a = "[$0]"

-- Update element at index
updateAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
updateAt i x xs = updateAtImpl i x xs

foreign import updateAtImpl :: forall a. Int -> a -> Array a -> Maybe (Array a) = "let <Idx> = $0 in let <Len> = call 'erlang':'length'($2) in\n        case call 'erlang':'or'(call 'erlang':'<'(Idx, 0), call 'erlang':'>='(Idx, Len)) of\n          <'true'> when 'true' -> 'Nothing'\n          <'false'> when 'true' -> case call 'lists':'split'(Idx, $2) of <{Before, After}> when 'true' ->\n            {'Just', call 'lists':'append'(Before, [$1|call 'erlang':'tl'(After)])} end\n        end"

-- Delete element at index
deleteAt :: forall a. Int -> Array a -> Maybe (Array a)
deleteAt i xs = deleteAtImpl i xs

foreign import deleteAtImpl :: forall a. Int -> Array a -> Maybe (Array a) = "let <Idx> = $0 in let <Len> = call 'erlang':'length'($1) in\n        case call 'erlang':'or'(call 'erlang':'<'(Idx, 0), call 'erlang':'>='(Idx, Len)) of\n          <'true'> when 'true' -> 'Nothing'\n          <'false'> when 'true' -> case call 'lists':'split'(Idx, $1) of <{Before, After}> when 'true' ->\n            {'Just', call 'lists':'append'(Before, call 'erlang':'tl'(After))} end\n        end"

-- Insert element at index
insertAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
insertAt i x xs = insertAtImpl i x xs

foreign import insertAtImpl :: forall a. Int -> a -> Array a -> Maybe (Array a) = "let <Idx> = $0 in let <Len> = call 'erlang':'length'($2) in\n        case call 'erlang':'or'(call 'erlang':'<'(Idx, 0), call 'erlang':'>'(Idx, Len)) of\n          <'true'> when 'true' -> 'Nothing'\n          <'false'> when 'true' -> case call 'lists':'split'(Idx, $2) of <{Before, After}> when 'true' ->\n            {'Just', call 'lists':'append'(Before, [$1|After])} end\n        end"

-- Slice array from start to end indices
slice :: forall a. Int -> Int -> Array a -> Array a
slice start end xs = sliceImpl start end xs

foreign import sliceImpl :: forall a. Int -> Int -> Array a -> Array a = "call 'lists':'sublist'($2, call 'erlang':'+'($0, 1), call 'erlang':'-'($1, $0))"

-- Remove duplicates
nub :: forall a. Array a -> Array a
nub xs = nubImpl xs

foreign import nubImpl :: forall a. Array a -> Array a = "call 'lists':'usort'($0)"

-- Remove duplicates with custom equality
nubBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a
nubBy f xs = nubByImpl f xs

foreign import nubByImpl :: forall a. (a -> a -> Boolean) -> Array a -> Array a = "call 'lists':'usort'($1)"

-- Group consecutive equal elements
group :: forall a. Array a -> Array (Array a)
group xs = groupImpl xs

foreign import groupImpl :: forall a. Array a -> Array (Array a) = "letrec 'doGroup'/2 = fun (Lst, Acc) -> case Lst of\n        <[]> when 'true' -> case Acc of <[]> when 'true' -> [] <_> when 'true' -> [call 'lists':'reverse'(Acc)] end\n        <[H|T]> when 'true' -> case Acc of\n          <[]> when 'true' -> apply 'doGroup'/2 (T, [H])\n          <[Prev|_]> when 'true' -> case call 'erlang':'=='(H, Prev) of\n            <'true'> when 'true' -> apply 'doGroup'/2 (T, [H|Acc])\n            <'false'> when 'true' -> [call 'lists':'reverse'(Acc)|apply 'doGroup'/2 (T, [H])]\n          end\n        end\n      end in apply 'doGroup'/2 ($0, [])"

-- Group by custom equality
groupBy :: forall a. (a -> a -> Boolean) -> Array a -> Array (Array a)
groupBy f xs = groupByImpl f xs

foreign import groupByImpl :: forall a. (a -> a -> Boolean) -> Array a -> Array (Array a) = "letrec 'doGroup'/3 = fun (Eq, Lst, Acc) -> case Lst of\n        <[]> when 'true' -> case Acc of <[]> when 'true' -> [] <_> when 'true' -> [call 'lists':'reverse'(Acc)] end\n        <[H|T]> when 'true' -> case Acc of\n          <[]> when 'true' -> apply 'doGroup'/3 (Eq, T, [H])\n          <[Prev|_]> when 'true' -> case apply Eq (Prev, H) of\n            <'true'> when 'true' -> apply 'doGroup'/3 (Eq, T, [H|Acc])\n            <'false'> when 'true' -> [call 'lists':'reverse'(Acc)|apply 'doGroup'/3 (Eq, T, [H])]\n          end\n        end\n      end in apply 'doGroup'/3 ($0, $1, [])"

-- Take while predicate holds
takeWhile :: forall a. (a -> Boolean) -> Array a -> Array a
takeWhile f xs = takeWhileImpl f xs

foreign import takeWhileImpl :: forall a. (a -> Boolean) -> Array a -> Array a = "call 'lists':'takewhile'($0, $1)"

-- Find index of first element matching predicate
findIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex f xs = findIndexImpl f xs

foreign import findIndexImpl :: forall a. (a -> Boolean) -> Array a -> Maybe Int = "letrec 'findIdx'/3 = fun (P, N, Lst) -> case Lst of\n        <[]> when 'true' -> 'Nothing'\n        <[H|T]> when 'true' -> case apply P (H) of\n          <'true'> when 'true' -> {'Just', N}\n          <'false'> when 'true' -> apply 'findIdx'/3 (P, call 'erlang':'+'(N, 1), T)\n        end\n      end in apply 'findIdx'/3 ($0, 0, $1)"

-- Sort array (uses natural ordering)
sort :: forall a. Array a -> Array a
sort xs = sortImpl xs

foreign import sortImpl :: forall a. Array a -> Array a = "call 'lists':'sort'($0)"

-- Intercalate (join arrays with separator array)
intercalate :: forall a. Array a -> Array (Array a) -> Array a
intercalate sep xs = intercalateImpl sep xs

foreign import intercalateImpl :: forall a. Array a -> Array (Array a) -> Array a = "call 'lists':'join'($0, $1)"

-- Split at index
splitAt :: forall a. Int -> Array a -> { before :: Array a, after :: Array a }
splitAt i xs = splitAtImpl i xs

foreign import splitAtImpl :: forall a. Int -> Array a -> { before :: Array a, after :: Array a } = "case call 'lists':'split'($0, $1) of\n        <{Before, After}> when 'true' -> {'before', Before, 'after', After}\n      end"

-- Map with Maybe (filter and map combined)
mapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Array b
mapMaybe f xs = mapMaybeImpl f xs

foreign import mapMaybeImpl :: forall a b. (a -> Maybe b) -> Array a -> Array b = "letrec 'doMapMaybe'/2 = fun (F, Lst) -> case Lst of\n        <[]> when 'true' -> []\n        <[H|T]> when 'true' -> case apply F (H) of\n          <'Nothing'> when 'true' -> apply 'doMapMaybe'/2 (F, T)\n          <{'Just', V}> when 'true' -> [V|apply 'doMapMaybe'/2 (F, T)]\n        end\n      end in apply 'doMapMaybe'/2 ($0, $1)"

-- catMaybes - filter out Nothing values
catMaybes :: forall a. Array (Maybe a) -> Array a
catMaybes xs = mapMaybe (\x -> x) xs

-- nubByEq - remove duplicates using custom equality function
nubByEq :: forall a. (a -> a -> Boolean) -> Array a -> Array a
nubByEq f xs = nubByEqImpl f xs

foreign import nubByEqImpl :: forall a. (a -> a -> Boolean) -> Array a -> Array a = "call 'lists':'usort'($1)"

-- Convert from a List to an Array
fromFoldable :: forall a. List a -> Array a
fromFoldable xs = fromFoldableImpl xs

foreign import fromFoldableImpl :: forall a. List a -> Array a = "$0"

-- Convert from Array to List
toUnfoldable :: forall a. Array a -> List a
toUnfoldable xs = toUnfoldableImpl xs

foreign import toUnfoldableImpl :: forall a. Array a -> List a = "$0"
