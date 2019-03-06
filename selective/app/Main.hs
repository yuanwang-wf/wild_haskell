module Main where

import Control.Applicative (liftA3)

pingPongM :: IO ()
pingPongM = getLine >>= (\ s -> if s == "ping" then putStrLn "pong" else return ())
-- getLine >>= Control.Monad.when( s == "ping") $ putStrLn "pong"

pingPongA :: IO ()
pingPongA =  (\ s -> id) <$> getLine <*> putStrLn "pong"

pingPongIfElseA :: IO ()
pingPongIfElseA =  liftA3 (\ s a b -> if s == "ping" then a else b )  getLine  (putStrLn "pong") (pure ())

whenS :: IO Bool -> IO () -> IO ()
whenS mb action = mb >>= \ b -> if b then action else return ()

pingPongS :: IO ()
pingPongS = whenS (fmap (== "ping") getLine) (putStrLn "pong")

main :: IO ()
main = pingPongM
