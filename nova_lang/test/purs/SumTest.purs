module Test.SumTest where

sum :: Array Int -> Int
sum xs = case xs of
  [] -> 0
  (h : t) -> h + sum t

main :: Int
main = sum [1, 2, 3, 4, 5]
