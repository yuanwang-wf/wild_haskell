module Passphrase where

import Data.Char (isAlpha, isNumber, isPunctuation)
import Data.Maybe (isJust)
import Control.Applicative (Alternative, empty, (<|>), liftA2)
import Control.Monad (MonadPlus, mzero, mplus, liftM, guard)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)

isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

-- getPassphrase :: IO (Maybe String)
-- getPassphrase = do s <- getLine
--                    if isValid s then return $ Just s
--                                 else return Nothing


-- askPassphrase :: IO ()
-- askPassphrase = do putStrLn "Insert your new passphrase:"
--                    maybeValue <- getPassphrase
--                    case maybeValue of
--                     Just value -> putStrLn "good password"
--                     Nothing -> putStrLn "passpharse invalid"


newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT g) = MaybeT $ (fmap . fmap) f g

instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . Just
    MaybeT mf <*> MaybeT ma = MaybeT $ pure (<*>) <*> mf <*> ma

instance Monad m => Monad (MaybeT m) where
    MaybeT ma >>= g = MaybeT $ ma >>= \ maybeA ->
        case maybeA of
            Nothing -> pure Nothing
            Just a -> runMaybeT $ g a

-- instance Monad m => Alternative (MaybeT m) where
--     empty    = MaybeT $ pure Nothing
--     MaybeT x <|> MaybeT y = MaybeT $ x >>= \ maybeA ->
--         case maybeA of
--             Nothing -> y
--             otherwise -> x

instance Applicative m => Alternative (MaybeT m) where
    empty    = MaybeT $ pure Nothing
    MaybeT x <|> MaybeT y = MaybeT $ liftA2 (\ x y -> if isJust x then x else y ) x y


instance Monad m => MonadPlus (MaybeT m) where
    mzero = empty
    mplus = (<|>)

instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)


getPassphrase :: MaybeT IO String
getPassphrase = do s <- lift $ getLine
                   guard (isValid s)
                   return s


-- askPassphrase :: MaybeT IO ()
-- askPassphrase = do lift $ putStrLn "Insert your new passphrase:"
--                    value <- getPassphrase
--                    lift $ putStrLn "good password"

askPassphrase :: MaybeT IO ()
askPassphrase = do
    lift $ putStrLn "Insert your new passphrase:"
    value <- getPassphrase
    lift $ putStrLn "good password"