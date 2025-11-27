module ModuleA where

import Prelude

data Color = Red | Green | Blue

type Point = { x :: Int, y :: Int }

add :: Int -> Int -> Int
add a b = a + b
