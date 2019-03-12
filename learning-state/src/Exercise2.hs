module Exercise2 where

import Control.Monad.Except
-- Non-monad tranformer version
-- TODO can we traversable here
-- foldTerminateM :: Monad m => (b -> a -> m (Either b b)) -> b -> [a] -> m b
-- foldTerminateM f accum [] = pure accum
-- foldTerminateM f accum (x :xs) = f accum x >>= \ e ->
--     case e of
--         Left lValue -> pure lValue
--         Right rValue -> foldTerminateM f rValue xs

foldTerminateM :: Monad m => (b -> a -> m (Either b b)) -> b -> [a] -> m b
foldTerminateM = undefined

loudSumPositive :: [Int] -> IO Int
loudSumPositive =
        foldTerminateM go 0
    where
        go total x
          | x < 0 = do
              putStrLn "Found a negative, stopping"
              return $ Left total
          | otherwise = do
              putStrLn "Non-negative, continuing"
              let total' = total + x
              putStrLn $ "New total: " ++ show total'
              return $ Right total'

main' :: IO ()
main' = do
    res <- loudSumPositive [1, 2, 3, -1, 5]
    putStrLn $ "Result: " ++ show res