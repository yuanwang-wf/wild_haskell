{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppT where

import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.State        (get, put, MonadState)
import Control.Monad.Trans.State (state, StateT)
import System.Random
import System.IO (readLn)

newtype AppT a = AppT {
    unAppT :: ReaderT Int IO a
} deriving (Functor, Applicative, Monad, MonadReader Int,
            MonadIO)


guessNumber :: (MonadReader Int m, MonadIO m) => StateT StdGen m Int
guessNumber = state $ randomR (0, 100)


run :: (MonadReader Int m, MonadState StdGen m, MonadIO m) => m ()
run = do
    seed <- ask
    liftIO $ putStrLn "guess a number"
    guess <- liftIO (readLn :: IO Int)
    generator <- get
    liftIO $ putStrLn "done"
    let (secretNumber, generator') = randomR (0, 100) generator
    case compare guess secretNumber of
        LT -> (liftIO $ print "Too Small") >> run
        EQ -> liftIO $ print "You Win!"
        GT -> (liftIO $ print "Too Big!") >> run

run' :: (MonadReader Int m, MonadIO m) => m ()
run' = do
    seed <- ask
    liftIO $ putStrLn "guess a number"
    guess <- liftIO (readLn :: IO Int)
    let (secretNumber, generator') = (randomR (0, 100) (mkStdGen seed) :: (Int, StdGen))
    case compare guess secretNumber of
        LT -> (liftIO $ print "Too Small") >> run'
        EQ -> liftIO $ print "You Win!"
        GT -> (liftIO $ print "Too Big!") >> run'