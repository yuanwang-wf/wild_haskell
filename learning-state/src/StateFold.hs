module StateFold where

import Control.Monad.Trans.State
import Data.Maybe

foldState :: (b -> a -> b) -> b -> [a] -> b
foldState f accum0 list0 =
        execState (mapM_ go list0) accum0
    where
        go x = modify' (`f` x)

findState :: (a -> Bool) -> [a] -> Maybe a
findState f xs =
        execState (mapM_ go xs) Nothing
    where
        go x = modify (\ y -> if isJust y then y else (if f x then Just x else Nothing) )

findStateWithCounter :: (a -> Bool) -> [a] -> (Maybe a, Int)
findStateWithCounter f xs =
        execState (mapM_ go xs) (Nothing, 0)
    where
        go x = modify (\ (y, n) -> if isJust y then (y, n) else (if f x then (Just x, n +1) else (Nothing, n+1)))
