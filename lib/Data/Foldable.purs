module Data.Foldable where

-- Foldable type class (simulated via functions)
-- These work with any foldable type (Array, List, etc.)

-- Fold left (strict)
foldl :: forall a b t. (b -> a -> b) -> b -> t a -> b
foldl f acc xs = foldlImpl f acc xs

foreign import foldlImpl :: forall a b t. (b -> a -> b) -> b -> t a -> b

-- Fold right (lazy)
foldr :: forall a b t. (a -> b -> b) -> b -> t a -> b
foldr f acc xs = foldrImpl f acc xs

foreign import foldrImpl :: forall a b t. (a -> b -> b) -> b -> t a -> b

-- Monadic fold left
foldM :: forall m a b t. (b -> a -> m b) -> b -> t a -> m b
foldM f acc xs = foldMImpl f acc xs

foreign import foldMImpl :: forall m a b t. (b -> a -> m b) -> b -> t a -> m b

-- Traverse for side effects only
traverse_ :: forall m a b t. (a -> m b) -> t a -> m Unit
traverse_ f xs = traverse_Impl f xs

foreign import traverse_Impl :: forall m a b t. (a -> m b) -> t a -> m Unit

-- For each (flipped traverse_)
for_ :: forall m a b t. t a -> (a -> m b) -> m Unit
for_ xs f = for_Impl xs f

foreign import for_Impl :: forall m a b t. t a -> (a -> m b) -> m Unit

-- Check if any element satisfies predicate
any :: forall a t. (a -> Boolean) -> t a -> Boolean
any f xs = anyImpl f xs

foreign import anyImpl :: forall a t. (a -> Boolean) -> t a -> Boolean

-- Check if all elements satisfy predicate
all :: forall a t. (a -> Boolean) -> t a -> Boolean
all f xs = allImpl f xs

foreign import allImpl :: forall a t. (a -> Boolean) -> t a -> Boolean

-- Check if element is in foldable
elem :: forall a t. a -> t a -> Boolean
elem x xs = elemImpl x xs

foreign import elemImpl :: forall a t. a -> t a -> Boolean

-- Check if element is not in foldable
notElem :: forall a t. a -> t a -> Boolean
notElem x xs = notElemImpl x xs

foreign import notElemImpl :: forall a t. a -> t a -> Boolean

-- Find first element satisfying predicate
find :: forall a t. (a -> Boolean) -> t a -> Maybe a
find f xs = findImpl f xs

foreign import findImpl :: forall a t. (a -> Boolean) -> t a -> Maybe a

-- Get length
length :: forall a t. t a -> Int
length xs = lengthImpl xs

foreign import lengthImpl :: forall a t. t a -> Int

-- Check if empty
null :: forall a t. t a -> Boolean
null xs = nullImpl xs

foreign import nullImpl :: forall a t. t a -> Boolean

-- Sum all elements (requires Semiring)
sum :: forall a t. t a -> a
sum xs = sumImpl xs

foreign import sumImpl :: forall a t. t a -> a

-- Product of all elements (requires Semiring)
product :: forall a t. t a -> a
product xs = productImpl xs

foreign import productImpl :: forall a t. t a -> a

-- Maximum element (requires Ord)
maximum :: forall a t. t a -> Maybe a
maximum xs = maximumImpl xs

foreign import maximumImpl :: forall a t. t a -> Maybe a

-- Minimum element (requires Ord)
minimum :: forall a t. t a -> Maybe a
minimum xs = minimumImpl xs

foreign import minimumImpl :: forall a t. t a -> Maybe a

-- Intercalate (join with separator)
intercalate :: forall m t. m -> t m -> m
intercalate sep xs = intercalateImpl sep xs

foreign import intercalateImpl :: forall m t. m -> t m -> m

-- Convert foldable to array
toArray :: forall a t. t a -> Array a
toArray xs = toArrayImpl xs

foreign import toArrayImpl :: forall a t. t a -> Array a
