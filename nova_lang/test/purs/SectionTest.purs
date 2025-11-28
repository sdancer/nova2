module Test.SectionTest where

-- Test right section: (+ 1) creates fn x -> x + 1
rightSection :: Int -> Int
rightSection = (+ 1)

-- Test left section: (10 -) creates fn x -> 10 - x
leftSection :: Int -> Int
leftSection = (10 -)

-- Test bare operator section: (+) creates fn x y -> x + y
bareSection :: Int -> Int -> Int
bareSection = (+)

main :: Int
main = rightSection 5
