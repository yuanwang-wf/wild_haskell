module Main where

import Exercise1
import StateFold

main :: IO ()
main = runReaderT main' "Welcome to mtl"
