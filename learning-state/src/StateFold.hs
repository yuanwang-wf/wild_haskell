module StateFold where

import           Control.Applicative
import           Control.Monad.Trans.State
import qualified Data.Set                  as S
import Data.Maybe (isJust)

foldState :: (b -> a -> b) -> b -> [a] -> b
foldState f accum0 list0 =
        execState (mapM_ go list0) accum0
    where
        go x = modify (\ accum -> f accum x)


findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM f (head : tail) = f head >>= \ x -> if x then pure (Just head) else findM f tail

findA :: Applicative f => (a -> f Bool) -> [a] -> f (Maybe a)
findA _ [] = pure Nothing
findA f (head : tail) = liftA3 helper (f head) (pure (Just head)) (findA f tail)
        where helper flag h tail = if flag then h else tail

findState :: (a -> Bool) -> [a] -> Maybe a
findState pred xs =
        evalState (findA helper xs) pred
    where
        helper :: (a -> State (a -> Bool) Bool)
        helper item = state (\ s -> (s item, s))

firstRepeat :: Ord a => [a] -> Maybe a
firstRepeat ls =
        evalState (findA helper ls) S.empty
    where
        helper :: Ord a => (a -> State (S.Set a) Bool)
        helper item = state (\s -> (S.member item s, S.insert item s))

filtering :: Monad m => (a -> m Bool) -> [a] -> m [a]
filtering _ [] = pure []
filtering f (head : tail) = f head >>= \ x -> if x then liftA2 (:) (pure head) (filtering f tail) else filtering f tail

distinct :: Ord a => [a] -> [a]
distinct xs =
        evalState (filtering helper xs) S.empty
    where
        helper :: Ord a => (a -> State (S.Set a) Bool)
        helper item = state (\s -> (not (S.member item s), S.insert item s))


findDuplidate :: Ord a => [a] -> [a]
findDuplidate xs = undefined

findStateWithCounter :: (a -> Bool) -> [a] -> (Maybe a, Int)
findStateWithCounter f xs =
        execState (mapM_ go xs) (Nothing, 0)
    where
        go x = modify (\ (y, n) -> if isJust y then (y, n) else (if f x then (Just x, n +1) else (Nothing, n+1)))
