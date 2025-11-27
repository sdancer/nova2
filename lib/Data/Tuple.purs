module Data.Tuple where

-- Tuple type (2-tuple / pair)
data Tuple a b = Tuple a b

-- Extract first element
fst :: forall a b. Tuple a b -> a
fst (Tuple a _) = a

-- Extract second element
snd :: forall a b. Tuple a b -> b
snd (Tuple _ b) = b

-- Swap elements
swap :: forall a b. Tuple a b -> Tuple b a
swap (Tuple a b) = Tuple b a

-- Create a tuple
curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
curry f a b = f (Tuple a b)

-- Uncurry a function
uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
uncurry f (Tuple a b) = f a b

-- Functor map over second element
map :: forall a b c. (b -> c) -> Tuple a b -> Tuple a c
map f (Tuple a b) = Tuple a (f b)
