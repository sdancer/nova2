module Data.Maybe where

-- Maybe type for optional values
data Maybe a = Nothing | Just a

-- Extract value with default
fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

-- Check if Maybe has a value
isJust :: forall a. Maybe a -> Boolean
isJust (Just _) = true
isJust Nothing = false

isNothing :: forall a. Maybe a -> Boolean
isNothing Nothing = true
isNothing (Just _) = false

-- Catamorphism for Maybe
maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe def _ Nothing = def
maybe _ f (Just x) = f x

-- Functor map for Maybe
map :: forall a b. (a -> b) -> Maybe a -> Maybe b
map _ Nothing = Nothing
map f (Just x) = Just (f x)

-- Applicative pure
pure :: forall a. a -> Maybe a
pure x = Just x

-- Bind for Maybe (monadic)
bind :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just x) f = f x
