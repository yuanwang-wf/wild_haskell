module Main where

import Control.Applicative (liftA3)

pingPongM :: IO ()
pingPongM = getLine >>= (\ s -> if s == "ping" then putStrLn "pong" else return ())
-- getLine >>= Control.Monad.when( s == "ping") $ putStrLn "pong"

pingPongA :: IO ()
pingPongA =  (\ s -> id) <$> getLine <*> putStrLn "pong"

pingPongIfElseA :: IO ()
pingPongIfElseA =  liftA3 (\ s a b -> if s == "ping" then a else b )  getLine  (putStrLn "pong") (pure ())

main :: IO ()
main = pingPongM
