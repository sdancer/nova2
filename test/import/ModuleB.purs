module ModuleB where

import Prelude
import ModuleA (Color(..), add)

isRed :: Color -> Boolean
isRed Red = true
isRed _ = false

useAdd :: Int -> Int
useAdd x = add x 1
