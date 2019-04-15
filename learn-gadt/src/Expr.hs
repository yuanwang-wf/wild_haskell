{-# LANGUAGE GADTs #-}
module Expr where

data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

i :: Int -> Expr Int
i = I

b :: Bool -> Expr Bool
b = B

add :: Expr Int -> Expr Int -> Expr Int
add = Add

