{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


module Trick where


import Control.Monad.Writer
import Data.String

t = execWriter (do tell "hello"; tell "world" :: Writer String ())

foo :: (a ~ ()) => a
foo = ()

instance () ~ a => IsString (Writer String a) where
    fromString = tell


data MyTuple a b = MyTuple a b
-- instance Show (MyTuple a b) where
--     show _ = "MyTuple <some value> <some value>"

instance (Show a, Show b) => Show (MyTuple a b) where
    show (MyTuple a b) = "MyTuple" ++ show a ++ show b