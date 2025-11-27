module Nova.Prelude where

-- Core types (these are primitives, defined in runtime)
-- Int, String, Char, Boolean, Array are built-in

-- Maybe type for optional values
data Maybe a = Nothing | Just a

-- Either type for success/failure
data Either a b = Left a | Right b

-- Unit type
data Unit = Unit

-- Tuple type (2-tuple)
data Tuple a b = Tuple a b

-- Basic tuple accessors
fst :: forall a b. Tuple a b -> a
fst (Tuple a _) = a

snd :: forall a b. Tuple a b -> b
snd (Tuple _ b) = b

-- Identity function
identity :: forall a. a -> a
identity x = x

-- Constant function
const :: forall a b. a -> b -> a
const x _ = x

-- Function composition
compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- Flip argument order
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

-- Maybe functions
fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

isJust :: forall a. Maybe a -> Boolean
isJust (Just _) = true
isJust Nothing = false

isNothing :: forall a. Maybe a -> Boolean
isNothing Nothing = true
isNothing (Just _) = false

maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe def _ Nothing = def
maybe _ f (Just x) = f x

-- Either functions
either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

isLeft :: forall a b. Either a b -> Boolean
isLeft (Left _) = true
isLeft (Right _) = false

isRight :: forall a b. Either a b -> Boolean
isRight (Right _) = true
isRight (Left _) = false

-- Boolean
otherwise :: Boolean
otherwise = true

-- Show class (simplified - just for common types)
show :: forall a. a -> String
show x = showImpl x

foreign import showImpl :: forall a. a -> String

-- Basic numeric operations (these map to runtime)
foreign import negate :: Int -> Int

-- List operations (convenience re-exports)
map :: forall a b. (a -> b) -> Array a -> Array b
map f xs = mapImpl f xs

foreign import mapImpl :: forall a b. (a -> b) -> Array a -> Array b

filter :: forall a. (a -> Boolean) -> Array a -> Array a
filter f xs = filterImpl f xs

foreign import filterImpl :: forall a. (a -> Boolean) -> Array a -> Array a

length :: forall a. Array a -> Int
length xs = lengthImpl xs

foreign import lengthImpl :: forall a. Array a -> Int

-- Append for arrays and strings (semigroup)
append :: forall a. Array a -> Array a -> Array a
append xs ys = appendImpl xs ys

foreign import appendImpl :: forall a. Array a -> Array a -> Array a

-- Fold operations
foldl :: forall a b. (b -> a -> b) -> b -> Array a -> b
foldl f acc xs = foldlImpl f acc xs

foreign import foldlImpl :: forall a b. (b -> a -> b) -> b -> Array a -> b

foldr :: forall a b. (a -> b -> b) -> b -> Array a -> b
foldr f acc xs = foldrImpl f acc xs

foreign import foldrImpl :: forall a b. (a -> b -> b) -> b -> Array a -> b
