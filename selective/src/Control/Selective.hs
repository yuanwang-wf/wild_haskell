module Control.Selective where

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


apS :: Selective f => f (a -> b) -> f a -> f b
apS f x = select (Left <$> f) (flip ($) <$> x)

whenS :: Selective f => f Bool -> f () -> f ()
whenS x y = selector <*? effect
    where
        selector = bool (Right ()) (Left ()) <$> x
        effect   = const                     <$> y