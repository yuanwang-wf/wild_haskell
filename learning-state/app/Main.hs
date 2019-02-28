module Main where

import StateFold

main :: IO ()
main = print $ findStateWithCounter (== 3) [1,2,4,5,13,3]
