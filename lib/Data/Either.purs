module Data.Either where

-- Either type for representing success/failure or two alternatives
data Either a b = Left a | Right b

-- Catamorphism for Either
either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

-- Check which variant
isLeft :: forall a b. Either a b -> Boolean
isLeft (Left _) = true
isLeft (Right _) = false

isRight :: forall a b. Either a b -> Boolean
isRight (Right _) = true
isRight (Left _) = false

-- Extract from Left
fromLeft :: forall a b. a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft def (Right _) = def

-- Extract from Right
fromRight :: forall a b. b -> Either a b -> b
fromRight _ (Right b) = b
fromRight def (Left _) = def

-- Functor map for Either (maps over Right)
map :: forall a b c. (b -> c) -> Either a b -> Either a c
map _ (Left a) = Left a
map f (Right b) = Right (f b)

-- Applicative pure
pure :: forall a b. b -> Either a b
pure x = Right x

-- Bind for Either (monadic)
bind :: forall a b c. Either a b -> (b -> Either a c) -> Either a c
bind (Left a) _ = Left a
bind (Right b) f = f b
