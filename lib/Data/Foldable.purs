module Data.Foldable where

-- Foldable type class (simulated via functions)
-- These work with any foldable type (Array, List, etc.)

-- Fold left (strict)
foldl :: forall a b t. (b -> a -> b) -> b -> t a -> b
foldl f acc xs = foldlImpl f acc xs

foreign import foldlImpl :: forall a b t. (b -> a -> b) -> b -> t a -> b = "call 'lists':'foldl'(fun (E, A) -> apply $0 (A, E), $1, $2)"

-- Fold right (lazy)
foldr :: forall a b t. (a -> b -> b) -> b -> t a -> b
foldr f acc xs = foldrImpl f acc xs

foreign import foldrImpl :: forall a b t. (a -> b -> b) -> b -> t a -> b = "call 'lists':'foldr'(fun (E, A) -> apply $0 (E, A), $1, $2)"

-- Monadic fold left
foldM :: forall m a b t. (b -> a -> m b) -> b -> t a -> m b
foldM f acc xs = foldMImpl f acc xs

foreign import foldMImpl :: forall m a b t. (b -> a -> m b) -> b -> t a -> m b = "letrec 'go'/2 = fun (A, L) -> case L of <[]> when 'true' -> {'Right', A} <[H|T]> when 'true' -> case apply $0 (A, H) of <{'Left', Err}> when 'true' -> {'Left', Err} <{'Right', NewA}> when 'true' -> apply 'go'/2 (NewA, T) end end in apply 'go'/2 ($1, $2)"

-- Traverse for side effects only
traverse_ :: forall m a b t. (a -> m b) -> t a -> m Unit
traverse_ f xs = traverse_Impl f xs

foreign import traverse_Impl :: forall m a b t. (a -> m b) -> t a -> m Unit = "let <_Ignored> = call 'lists':'foreach'(fun (X) -> apply $0 (X), $1) in 'unit'"

-- For each (flipped traverse_)
for_ :: forall m a b t. t a -> (a -> m b) -> m Unit
for_ xs f = for_Impl xs f

foreign import for_Impl :: forall m a b t. t a -> (a -> m b) -> m Unit = "let <_Ignored> = call 'lists':'foreach'(fun (X) -> apply $1 (X), $0) in 'unit'"

-- Check if any element satisfies predicate
any :: forall a t. (a -> Boolean) -> t a -> Boolean
any f xs = anyImpl f xs

foreign import anyImpl :: forall a t. (a -> Boolean) -> t a -> Boolean = "call 'lists':'any'(fun (X) -> apply $0 (X), $1)"

-- Check if all elements satisfy predicate
all :: forall a t. (a -> Boolean) -> t a -> Boolean
all f xs = allImpl f xs

foreign import allImpl :: forall a t. (a -> Boolean) -> t a -> Boolean = "call 'lists':'all'(fun (X) -> apply $0 (X), $1)"

-- Check if element is in foldable
elem :: forall a t. a -> t a -> Boolean
elem x xs = elemImpl x xs

foreign import elemImpl :: forall a t. a -> t a -> Boolean = "call 'lists':'member'($0, $1)"

-- Check if element is not in foldable
notElem :: forall a t. a -> t a -> Boolean
notElem x xs = notElemImpl x xs

foreign import notElemImpl :: forall a t. a -> t a -> Boolean = "call 'erlang':'not'(call 'lists':'member'($0, $1))"

-- Find first element satisfying predicate
find :: forall a t. (a -> Boolean) -> t a -> Maybe a
find f xs = findImpl f xs

foreign import findImpl :: forall a t. (a -> Boolean) -> t a -> Maybe a = "case call 'lists':'search'(fun (X) -> apply $0 (X), $1) of <{'value', V}> when 'true' -> {'Just', V} <'false'> when 'true' -> 'Nothing' end"

-- Get length
length :: forall a t. t a -> Int
length xs = lengthImpl xs

foreign import lengthImpl :: forall a t. t a -> Int = "call 'erlang':'length'($0)"

-- Check if empty
null :: forall a t. t a -> Boolean
null xs = nullImpl xs

foreign import nullImpl :: forall a t. t a -> Boolean = "call 'erlang':'=:='($0, [])"

-- Sum all elements (requires Semiring)
sum :: forall a t. t a -> a
sum xs = sumImpl xs

foreign import sumImpl :: forall a t. t a -> a = "call 'lists':'sum'($0)"

-- Product of all elements (requires Semiring)
product :: forall a t. t a -> a
product xs = productImpl xs

foreign import productImpl :: forall a t. t a -> a = "call 'lists':'foldl'(fun (X, Acc) -> call 'erlang':'*'(X, Acc), 1, $0)"

-- Maximum element (requires Ord)
maximum :: forall a t. t a -> Maybe a
maximum xs = maximumImpl xs

foreign import maximumImpl :: forall a t. t a -> Maybe a = "case $0 of <[]> when 'true' -> 'Nothing' <_> when 'true' -> {'Just', call 'lists':'max'($0)} end"

-- Minimum element (requires Ord)
minimum :: forall a t. t a -> Maybe a
minimum xs = minimumImpl xs

foreign import minimumImpl :: forall a t. t a -> Maybe a = "case $0 of <[]> when 'true' -> 'Nothing' <_> when 'true' -> {'Just', call 'lists':'min'($0)} end"

-- Intercalate (join with separator)
intercalate :: forall m t. m -> t m -> m
intercalate sep xs = intercalateImpl sep xs

foreign import intercalateImpl :: forall m t. m -> t m -> m = "call 'lists':'join'($0, $1)"

-- Convert foldable to array
toArray :: forall a t. t a -> Array a
toArray xs = toArrayImpl xs

foreign import toArrayImpl :: forall a t. t a -> Array a = "$0"
