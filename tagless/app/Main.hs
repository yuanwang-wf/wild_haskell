module Main where

import Lib

main :: IO ()
main = do
    (unNoCache $ requestData "john") >>= (putStrLn . show)
    (unInCache $ requestData "john") >>= (putStrLn . show)
