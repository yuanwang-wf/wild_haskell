-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}

module Exercise1 where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT g) = ReaderT ( fmap f . g)

instance Applicative f => Applicative (ReaderT r f) where
    pure a = ReaderT (\ r -> pure a)
    ReaderT g <*> ReaderT h = ReaderT (\ r -> g r <*> (h r))

instance Monad m => Monad (ReaderT f m) where
    ReaderT g >>= f = ReaderT (\ r -> g r >>= ( \ a -> runReaderT (f a) r))

instance MonadTrans (ReaderT r) where
    lift action = ReaderT $ const action

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

type Reader r = ReaderT r Identity

runReader :: Reader r a -> r -> a
runReader r = runIdentity . runReaderT r

ask :: Monad m => ReaderT r m r
ask = ReaderT pure

-- main :: IO ()
-- main = runReaderT main' "Hello World"

main' :: ReaderT String IO ()
main' = do
    lift $ putStrLn "I'm going to tell you a message"
    liftIO $ putStrLn "The message is:"
    message <- ask
    lift $ putStrLn message