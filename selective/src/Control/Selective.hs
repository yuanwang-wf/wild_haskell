module Control.Selective where

import Control.Applicative (liftA2, liftA3)
import Data.Bool (bool)

class Applicative f => Selective f where
    select :: f (Either a b) -> f (a -> b) -> f b
    select = selectA

    (<*?) :: f (Either a b) -> f (a -> b) -> f b
    (<*?) = select


selectA :: Applicative f => f (Either a b) -> f (a -> b) -> f b
selectA x y = (\ e f -> either f id e) <$> x <*> y

selectM :: Monad f => f (Either a b) -> f (a -> b) -> f b
selectM x y = x >>= \ e -> case e of Left a -> ($ a) <$> y
                                     Right b -> pure b


apDefault :: Selective f => f (a -> b) -> f a -> f b
apDefault f x = select (Left <$> f) (flip ($) <$> x)

whenS :: Selective f => f Bool -> f () -> f ()
whenS x y = selector <*? effect
    where
        selector = bool (Right ()) (Left ()) <$> x
        effect   = const                     <$> y


branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch e l r = fmap (fmap Left) e <*? fmap (fmap Right) l <*? r

ifS :: Selective f => f Bool -> f a -> f a -> f a
ifS fp fl fr = liftA3 (\ p l r -> if p then Left l else Right r ) fp fl fr <*? pure id